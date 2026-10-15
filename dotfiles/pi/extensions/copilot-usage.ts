/**
 * Copilot usage — shows GitHub Copilot premium-request usage in the footer
 * whenever the active model's provider is `github-copilot`.
 *
 * Uses the GitHub OAuth token pi stores in auth.json (`github-copilot.refresh`)
 * and queries https://api.github.com/copilot_internal/user.
 */
import { readFile } from "node:fs/promises";
import { join } from "node:path";
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
			ctx.ui.setStatus(STATUS_KEY, theme.fg("dim", "Copilot: unlimited"));
			return;
		}
		const used = Math.max(0, 100 - snap.percent_remaining);
		const color = used >= 90 ? "error" : used >= 75 ? "warning" : "success";
		const text =
			theme.fg("dim", "Copilot ") +
			theme.fg(color, `${used.toFixed(1)}%`) +
			theme.fg("dim", ` (${snap.entitlement - snap.remaining}/${snap.entitlement})`);
		ctx.ui.setStatus(STATUS_KEY, text);
	}

	async function update(ctx: ExtensionContext, force = false) {
		if (!ctx.hasUI) return;
		if (ctx.model?.provider !== PROVIDER) {
			ctx.ui.setStatus(STATUS_KEY, undefined);
			return;
		}
		render(ctx); // show cached value immediately
		if (!force && Date.now() - lastFetch < MIN_REFRESH_MS) return;
		inFlight ??= fetchUsage()
			.then(() => {
				if (ctx.model?.provider === PROVIDER) render(ctx);
			})
			.catch((err) => {
				if (!cached) ctx.ui.setStatus(STATUS_KEY, ctx.ui.theme.fg("dim", `Copilot: ? (${err.message})`));
			})
			.finally(() => {
				inFlight = undefined;
			});
		await inFlight;
	}

	pi.on("session_start", async (_e, ctx) => void update(ctx));
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
