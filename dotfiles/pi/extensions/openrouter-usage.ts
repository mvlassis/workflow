/**
 * OpenRouter usage — shows the active key's effective spending-budget usage
 * in the footer whenever the active model's provider is `openrouter`.
 *
 * Uses the API key configured in pi and queries GET /api/v1/key. Newer
 * OpenRouter responses include `effective_budget`; older responses fall back
 * to the key's own `limit` and `limit_remaining` fields.
 */
import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";

const STATUS_KEY = "openrouter-usage";
const PROVIDER = "openrouter";
const API_URL = "https://openrouter.ai/api/v1/key";
const MIN_REFRESH_MS = 60_000; // don't hit the API more than once per minute

type Budget = {
	limit_usd: number;
	spend_usd: number;
	remaining_usd: number;
	reset_interval?: string | null;
	resets_at?: string | null;
};

type KeyInfo = {
	effective_budget?: Budget | null;
	limit?: number | null;
	limit_remaining?: number | null;
	limit_reset?: string | null;
	usage?: number;
};

type UsageSnapshot = {
	used: number;
	limit?: number;
	remaining?: number;
	resetInterval?: string | null;
	resetDate?: string | null;
};

function isFiniteNumber(value: unknown): value is number {
	return typeof value === "number" && Number.isFinite(value);
}

function parseUsage(data: KeyInfo): UsageSnapshot {
	const budget = data.effective_budget;
	if (
		budget &&
		isFiniteNumber(budget.limit_usd) &&
		isFiniteNumber(budget.spend_usd) &&
		isFiniteNumber(budget.remaining_usd)
	) {
		return {
			used: Math.max(0, budget.spend_usd),
			limit: Math.max(0, budget.limit_usd),
			remaining: Math.max(0, budget.remaining_usd),
			resetInterval: budget.reset_interval,
			resetDate: budget.resets_at,
		};
	}

	if (isFiniteNumber(data.limit) && isFiniteNumber(data.limit_remaining)) {
		const limit = Math.max(0, data.limit);
		const remaining = Math.max(0, data.limit_remaining);
		return {
			used: Math.max(0, limit - data.limit_remaining),
			limit,
			remaining,
			resetInterval: data.limit_reset,
		};
	}

	if (isFiniteNumber(data.usage)) return { used: Math.max(0, data.usage) };
	throw new Error("no usage data");
}

function usd(value: number): string {
	return `$${value.toFixed(2)}`;
}

function percentUsed(snap: UsageSnapshot): number {
	if (!snap.limit) return 100;
	return Math.max(0, (snap.used / snap.limit) * 100);
}

export default function (pi: ExtensionAPI) {
	let lastFetch = 0;
	let cached: UsageSnapshot | undefined;
	let inFlight: Promise<void> | undefined;
	let lastError: string | undefined;

	async function fetchUsage(ctx: ExtensionContext): Promise<void> {
		const apiKey = await ctx.modelRegistry.getApiKeyForProvider(PROVIDER);
		if (!apiKey) throw new Error("not logged in");

		const res = await fetch(API_URL, {
			headers: {
				Accept: "application/json",
				Authorization: `Bearer ${apiKey}`,
			},
			signal: AbortSignal.timeout(5000),
		});
		if (!res.ok) {
			let message = `HTTP ${res.status}`;
			try {
				const body = (await res.json()) as { error?: { message?: unknown } };
				if (typeof body.error?.message === "string") message += `: ${body.error.message}`;
			} catch {
				// Keep the HTTP status when the response has no JSON error body.
			}
			throw new Error(message);
		}

		const body = (await res.json()) as { data?: KeyInfo };
		if (!body.data) throw new Error("invalid API response");
		cached = parseUsage(body.data);
		lastFetch = Date.now();
	}

	function render(ctx: ExtensionContext) {
		if (!cached) return;
		const theme = ctx.ui.theme;

		if (cached.limit === undefined) {
			ctx.ui.setStatus(STATUS_KEY, theme.fg("dim", `OpenRouter ${usd(cached.used)} used`));
			return;
		}

		const used = percentUsed(cached);
		const color = used >= 90 ? "error" : used >= 75 ? "warning" : "success";
		const text =
			theme.fg("dim", "OpenRouter ") +
			theme.fg(color, `${used.toFixed(1)}%`) +
			theme.fg("dim", ` (${usd(cached.used)}/${usd(cached.limit)})`);
		ctx.ui.setStatus(STATUS_KEY, text);
	}

	async function refresh(ctx: ExtensionContext): Promise<void> {
		inFlight ??= fetchUsage(ctx)
			.then(() => {
				lastError = undefined;
				if (ctx.model?.provider === PROVIDER) render(ctx);
			})
			.catch((err) => {
				lastError = err instanceof Error ? err.message : String(err);
				if (!cached && ctx.model?.provider === PROVIDER) {
					ctx.ui.setStatus(STATUS_KEY, ctx.ui.theme.fg("dim", `OpenRouter: ? (${lastError})`));
				}
			})
			.finally(() => {
				inFlight = undefined;
			});
		await inFlight;
	}

	async function update(ctx: ExtensionContext, force = false) {
		if (!ctx.hasUI) return;
		if (ctx.model?.provider !== PROVIDER) {
			ctx.ui.setStatus(STATUS_KEY, undefined);
			return;
		}
		render(ctx); // show cached value immediately
		if (!force && Date.now() - lastFetch < MIN_REFRESH_MS) return;
		await refresh(ctx);
	}

	pi.on("session_start", async (_e, ctx) => {
		void update(ctx);
	});
	pi.on("model_select", async (_e, ctx) => void update(ctx));
	pi.on("agent_end", async (_e, ctx) => void update(ctx));

	pi.registerCommand("openrouter-usage", {
		description: "Refresh OpenRouter spending-budget usage",
		handler: async (_args, ctx) => {
			if (!ctx.hasUI) return;
			await refresh(ctx);
			if (cached) {
				let message: string;
				if (cached.limit === undefined) {
					message = `OpenRouter all-time usage: ${usd(cached.used)}; no spending limit configured`;
				} else {
					const interval = cached.resetInterval ? `${cached.resetInterval} ` : "";
					message =
						`OpenRouter ${interval}budget: ${percentUsed(cached).toFixed(1)}% used, ` +
						`${usd(cached.remaining ?? 0)} remaining of ${usd(cached.limit)}` +
						(cached.resetDate ? `, resets ${cached.resetDate}` : "");
				}
				ctx.ui.notify(message + (lastError ? ` (refresh failed: ${lastError}; showing cached data)` : ""), lastError ? "warning" : "info");
			} else {
				ctx.ui.notify(`OpenRouter usage unavailable: ${lastError ?? "unknown error"}`, "error");
			}
		},
	});
}
