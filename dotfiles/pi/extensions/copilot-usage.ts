/**
 * Copilot usage — shows GitHub Copilot premium-request usage in the footer
 * whenever the active model's provider is `github-copilot`.
 *
 * Uses the GitHub OAuth token pi stores in auth.json (`github-copilot.refresh`)
 * and queries https://api.github.com/copilot_internal/user.
 */
import { readFile } from "node:fs/promises";
import { join } from "node:path";
import { truncateToWidth, visibleWidth } from "@earendil-works/pi-tui";
import { getAgentDir, type ExtensionAPI, type ExtensionContext } from "@earendil-works/pi-coding-agent";

const STATUS_KEY = "copilot-usage";
const PROVIDER = "github-copilot";
const MIN_REFRESH_MS = 60_000; // don't hit the API more than once per minute

type QuotaSnapshot = {
	percent_remaining: number;
	remaining: number;
	entitlement: number;
	unlimited: boolean;
};

export default function (pi: ExtensionAPI) {
	let lastFetch = 0;
	let cached: { snap: QuotaSnapshot; resetDate?: string } | undefined;
	let inFlight: Promise<void> | undefined;
	let statusText: string | undefined;
	let requestRender: (() => void) | undefined;

	// Replace ctx.ui.setStatus: keep text locally and render it at the top-right of the footer.
	const setStatus = (_key: string, text: string | undefined) => {
		statusText = text;
		requestRender?.();
	};

	function installFooter(ctx: ExtensionContext) {
		if (!ctx.hasUI) return;
		ctx.ui.setFooter((tui, theme, footerData) => {
			requestRender = () => tui.requestRender();
			const unsub = footerData.onBranchChange(() => tui.requestRender());
			const fmt = (n: number) =>
				n < 1000 ? `${n}` : n < 10000 ? `${(n / 1000).toFixed(1)}k` : n < 1e6 ? `${Math.round(n / 1000)}k` : `${(n / 1e6).toFixed(1)}M`;
			return {
				dispose() {
					unsub();
					requestRender = undefined;
				},
				invalidate() {},
				render(width: number): string[] {
					let input = 0, output = 0, cacheRead = 0, cacheWrite = 0, cost = 0;
					for (const e of ctx.sessionManager.getEntries()) {
						if (e.type === "message" && e.message.role === "assistant") {
							const u = (e.message as any).usage;
							input += u.input; output += u.output;
							cacheRead += u.cacheRead; cacheWrite += u.cacheWrite;
							cost += u.cost.total;
						}
					}
					// Line 1: cwd (branch) • name ........ copilot usage
					const home = process.env.HOME;
					let pwd = ctx.sessionManager.getCwd();
					if (home && (pwd === home || pwd.startsWith(home + "/"))) pwd = "~" + pwd.slice(home.length);
					const branch = footerData.getGitBranch();
					if (branch) pwd += ` (${branch})`;
					const name = ctx.sessionManager.getSessionName();
					if (name) pwd += ` • ${name}`;
					const right = statusText ?? "";
					const rw = visibleWidth(right);
					const leftW = Math.max(0, width - rw - (rw ? 2 : 0));
					const left = truncateToWidth(theme.fg("dim", pwd), leftW, theme.fg("dim", "..."));
					const line1 = left + " ".repeat(Math.max(0, width - visibleWidth(left) - rw)) + right;

					// Line 2: token stats ........ model
					const parts: string[] = [];
					if (input) parts.push(`↑${fmt(input)}`);
					if (output) parts.push(`↓${fmt(output)}`);
					if (cacheRead) parts.push(`R${fmt(cacheRead)}`);
					if (cacheWrite) parts.push(`W${fmt(cacheWrite)}`);
					if (cost) parts.push(`$${cost.toFixed(3)}`);
					const cu = ctx.getContextUsage();
					const win = cu?.contextWindow ?? ctx.model?.contextWindow ?? 0;
					const pct = cu?.percent;
					const pctStr = `${pct == null ? "?" : pct.toFixed(1)}%/${fmt(win)}`;
					parts.push(pct != null && pct > 90 ? theme.fg("error", pctStr) : pct != null && pct > 70 ? theme.fg("warning", pctStr) : pctStr);
					let model = ctx.model?.id ?? "no-model";
					if (ctx.model?.reasoning) model += ` • ${pi.getThinkingLevel()}`;
					const l2 = truncateToWidth(parts.join(" "), width, "...");
					const pad = " ".repeat(Math.max(2, width - visibleWidth(l2) - visibleWidth(model)));
					const line2 = truncateToWidth(theme.fg("dim", l2) + theme.fg("dim", pad + model), width, "");

					const lines = [line1, line2];
					// Other extensions' statuses (excluding ours, which isn't set via the real setStatus)
					const others = [...footerData.getExtensionStatuses().entries()]
						.sort(([a], [b]) => a.localeCompare(b))
						.map(([, t]) => t.replace(/[\r\n\t]/g, " ").replace(/ +/g, " ").trim());
					if (others.length) lines.push(truncateToWidth(others.join(" "), width, theme.fg("dim", "...")));
					return lines;
				},
			};
		});
	}

	async function getGitHubToken(): Promise<{ token: string; domain: string } | undefined> {
		const raw = await readFile(join(getAgentDir(), "auth.json"), "utf8");
		const cred = JSON.parse(raw)?.[PROVIDER];
		if (!cred?.refresh) return undefined;
		const domain = typeof cred.enterpriseUrl === "string" && cred.enterpriseUrl ? cred.enterpriseUrl : "github.com";
		return { token: cred.refresh, domain };
	}

	async function fetchUsage(): Promise<void> {
		const auth = await getGitHubToken();
		if (!auth) throw new Error("not logged in");
		const res = await fetch(`https://api.${auth.domain}/copilot_internal/user`, {
			headers: {
				Accept: "application/json",
				Authorization: `token ${auth.token}`,
				"Editor-Version": "vscode/1.107.0",
				"User-Agent": "GitHubCopilotChat/0.35.0",
			},
			signal: AbortSignal.timeout(5000),
		});
		if (!res.ok) throw new Error(`HTTP ${res.status}`);
		const data: any = await res.json();
		const snap = data?.quota_snapshots?.premium_interactions as QuotaSnapshot | undefined;
		if (!snap) throw new Error("no quota data");
		cached = { snap, resetDate: data.quota_reset_date };
		lastFetch = Date.now();
	}

	function render(ctx: ExtensionContext) {
		const theme = ctx.ui.theme;
		if (!cached) return;
		const { snap } = cached;
		if (snap.unlimited) {
			setStatus(STATUS_KEY, theme.fg("dim", "Copilot: unlimited"));
			return;
		}
		const used = Math.max(0, 100 - snap.percent_remaining);
		const color = used >= 90 ? "error" : used >= 75 ? "warning" : "success";
		const text =
			theme.fg("dim", "Copilot ") +
			theme.fg(color, `${used.toFixed(1)}%`) +
			theme.fg("dim", ` (${snap.entitlement - snap.remaining}/${snap.entitlement})`);
		setStatus(STATUS_KEY, text);
	}

	async function update(ctx: ExtensionContext, force = false) {
		if (!ctx.hasUI) return;
		if (ctx.model?.provider !== PROVIDER) {
			setStatus(STATUS_KEY, undefined);
			return;
		}
		render(ctx); // show cached value immediately
		if (!force && Date.now() - lastFetch < MIN_REFRESH_MS) return;
		inFlight ??= fetchUsage()
			.then(() => {
				if (ctx.model?.provider === PROVIDER) render(ctx);
			})
			.catch((err) => {
				if (!cached) setStatus(STATUS_KEY, ctx.ui.theme.fg("dim", `Copilot: ? (${err.message})`));
			})
			.finally(() => {
				inFlight = undefined;
			});
		await inFlight;
	}

	pi.on("session_start", async (_e, ctx) => {
		installFooter(ctx);
		void update(ctx);
	});
	pi.on("model_select", async (_e, ctx) => void update(ctx));
	pi.on("agent_end", async (_e, ctx) => void update(ctx));

	pi.registerCommand("copilot-usage", {
		description: "Refresh GitHub Copilot premium request usage",
		handler: async (_args, ctx) => {
			await update(ctx, true);
			if (cached) {
				const { snap, resetDate } = cached;
				ctx.ui.notify(
					`Copilot premium requests: ${(100 - snap.percent_remaining).toFixed(1)}% used, ` +
						`${snap.remaining}/${snap.entitlement} remaining${resetDate ? `, resets ${resetDate}` : ""}`,
					"info",
				);
			}
		},
	});
}
