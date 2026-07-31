# ============================================================================
# 19_migration_position_trajectories.R — "moved apart or moved together?"
# ----------------------------------------------------------------------------
# DESCRIPTIVE, EXPLORATORY (directional B layer; same status as H2b/H2c/H3).
#
# WHY THIS EXISTS. The Dalton dispersion used by H2c/H3 is MEAN-INVARIANT: if
# every party shifts by the same amount, the measure does not move at all. A
# modest rise in migration polarisation is therefore compatible with two very
# different worlds:
#     (a) the whole chamber drifts towards one pole  (level shifts, spread flat)
#     (b) the parties fan out                        (spread rises, level need not move)
# 16_H3.R already writes the quantity that separates them (mean_pos ->
# H3_mean_positions.csv), but only the Europe row is ever read out. This script
# disaggregates the migration domain to the PARTY level and puts level and
# spread on one page.
#
# NO NEW MEASURE, NO NEW HYPOTHESIS. Same polarization_score(), same
# dalton_polarization(), same weights, same specification as H3. This is a
# read-out of quantities the pipeline already produces.
#
# Inputs : H2b_polarization_table.csv   (party x lp x domain, both channels)
#          H3_system_polarization.csv   (canonical weighted mean + dispersion)
#          H2c_dispersion_table.csv     (canonical all vs excl-AfD dispersion)
#          SEAT_SHARES (00_config.R)
# Outputs: migration_trajectories.csv
#          figures/migration_trajectories.{pdf,png}
#
# ---------------------------------------------------------------------------
# CELL-BASIS NOTE — RESOLVED 31.07.2026.
#   Found 29.07.2026: H2b_polarization_table.csv ran on the 38-cell basis and
#   contained NO Linke row for LP 13-15, while the H3/H2c series counted FIVE
#   parties in those periods, because their party set came from the weight-table
#   joins alone and the PDS carries positive vote and seat shares there. Panel A
#   and panel B of this figure therefore ran on different party sets.
#   Fixed 31.07.2026 in 15_H2.R (block 0b) and 16_H3.R (block 1b): the
#   party-continuity exclusion is now applied to the position scores before the
#   weighting, so both panels run on the same base. The reconciliation check at
#   the bottom of this script is the regression test for that fix and must
#   report PASS. If it ever reports FAIL, the exclusion has been lost upstream —
#   do not publish the figure until it passes again.
# ============================================================================

source(here::here("00_config.R"))
suppressPackageStartupMessages({ library(tidyverse); library(patchwork) })

chk  <- function(cond, msg) if (!isTRUE(cond)) stop("[19][CHECK FAILED] ", msg, call. = FALSE)
flag <- function(cond, msg) if (!isTRUE(cond)) message("[19][FLAG] ", msg)

DOMAIN_FOCUS <- "Migration"
AFD_ENTRY_X  <- 18.5

need <- function(f) { p <- file.path(PATHS$out_dir, f); chk(file.exists(p), paste0("missing input: ", p)); p }

# ---- party-level trajectories (panel A) ------------------------------------
seats <- as_tibble(SEAT_SHARES) %>% mutate(lp = as.integer(lp))

traj <- read_csv(need("H2b_polarization_table.csv"), show_col_types = FALSE) %>%
  filter(domain == DOMAIN_FOCUS) %>%
  mutate(lp = as.integer(lp)) %>%
  select(party, lp, position = speech_pol) %>%
  inner_join(seats, by = c("party", "lp")) %>%
  filter(seat_share > 0, !is.na(position))
chk(nrow(traj) > 0, "no migration speech positions after the seat-share join")
flag(all(abs(traj$position) <= 1 + 1e-9), "position outside [-1,1]")

# ---- canonical system series (panel B) -------------------------------------
sys_mean <- read_csv(need("H3_system_polarization.csv"), show_col_types = FALSE) %>%
  filter(domain == DOMAIN_FOCUS, channel == "Speech") %>%
  transmute(lp = as.integer(lp), mean_pos, dispersion = polarization, n_parties)

sys_disp <- read_csv(need("H2c_dispersion_table.csv"), show_col_types = FALSE) %>%
  filter(domain == DOMAIN_FOCUS, str_starts(series, "Speech")) %>%
  transmute(lp = as.integer(lp),
            set = if_else(str_detect(series, "excl"), "Excl. AfD", "All parties"),
            dispersion = sd_polarization)

# excl-AfD MEAN is not exported anywhere, so derive it from the cell table.
# The AfD holds seats only in LP 19-20, where the 38-cell basis is complete, so
# this derivation is exact for the periods where the two series can differ.
afd_lps  <- traj %>% filter(party == "AfD") %>% pull(lp) %>% unique()
draw_lps <- sort(unique(c(min(afd_lps) - 1L, afd_lps)))
mean_ex <- traj %>% filter(party != "AfD", lp %in% draw_lps) %>%
  group_by(lp) %>%
  summarise(out = list(dalton_polarization(position, seat_share)), .groups = "drop") %>%
  unnest(out) %>% select(lp, mean_pos)

disp_all <- sys_disp %>% filter(set == "All parties")
disp_ex  <- sys_disp %>% filter(set == "Excl. AfD", lp %in% draw_lps)

# ---- export ----------------------------------------------------------------
bind_rows(
  traj     %>% transmute(lp, series = paste0("speech_", party), value = position),
  sys_mean %>% transmute(lp, series = "mean_all",   value = mean_pos),
  mean_ex  %>% transmute(lp, series = "mean_exAfD", value = mean_pos),
  disp_all %>% transmute(lp, series = "sd_all",     value = dispersion),
  sys_disp %>% filter(set == "Excl. AfD") %>% transmute(lp, series = "sd_exAfD", value = dispersion)
) %>% arrange(series, lp) %>%
  write_csv(file.path(PATHS$out_dir, "migration_trajectories.csv"))

# ============================================================================
# Figure
# ============================================================================
label_last <- traj %>% group_by(party) %>% slice_max(lp, n = 1) %>% ungroup()

pA <- traj %>%
  ggplot(aes(lp, position, colour = party, group = party)) +
  geom_hline(yintercept = 0, colour = "grey80") +
  geom_vline(xintercept = AFD_ENTRY_X, linetype = "dotted", colour = "grey50") +
  annotate("text", x = AFD_ENTRY_X, y = Inf, label = "  AfD enters (LP 19)",
           hjust = 0, vjust = 1.6, colour = "grey40", size = 3) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.0) +
  geom_text(data = label_last, aes(label = party), colour = "grey15",
            hjust = 0, nudge_x = 0.18, size = 3.2, show.legend = FALSE) +
  scale_colour_manual(values = PARTY_COLOURS, guide = "none") +
  scale_x_continuous(breaks = LEGISLATIVE_PERIODS, limits = c(12.8, 21.7)) +
  labs(title = "A  Party positions in speech",
       subtitle = str_wrap(paste("Five of six parties move towards the liberal pole,",
                                 "the CDU/CSU holds, the AfD enters beyond the previous range"), 90),
       x = "Legislative period", y = "Migration position (+ = restrictive)") +
  theme_thesis()

pB <- ggplot() +
  geom_hline(yintercept = 0, colour = "grey80") +
  geom_vline(xintercept = AFD_ENTRY_X, linetype = "dotted", colour = "grey50") +
  geom_line(data = sys_mean, aes(lp, mean_pos, colour = "Weighted mean"), linewidth = 0.8) +
  geom_point(data = sys_mean, aes(lp, mean_pos, colour = "Weighted mean"), size = 2.0) +
  geom_line(data = mean_ex, aes(lp, mean_pos, colour = "Weighted mean"),
            linewidth = 0.6, linetype = "dashed") +
  geom_line(data = disp_all, aes(lp, dispersion, colour = "Weighted dispersion"), linewidth = 0.8) +
  geom_point(data = disp_all, aes(lp, dispersion, colour = "Weighted dispersion"), size = 2.0) +
  geom_line(data = disp_ex, aes(lp, dispersion, colour = "Weighted dispersion"),
            linewidth = 0.6, linetype = "dashed") +
  scale_colour_manual(values = c("Weighted mean" = "#264653", "Weighted dispersion" = "#E76F51"),
                      name = NULL) +
  scale_x_continuous(breaks = LEGISLATIVE_PERIODS) +
  labs(title = "B  System level and system spread",
       subtitle = "Dashed = the same series with the AfD removed and the weights renormalised (Arm 6 logic)",
       x = "Legislative period", y = "Dalton-weighted mean / dispersion") +
  theme_thesis()

# NOTE (30.07.2026): the in-figure caption is baked into the PNG and cannot be
# renumbered later, so it carries NO section reference and NO status label.
# The numbered caption in the document does that work. Do not put "Section x.y"
# back in here.
p <- pA + pB +
  plot_annotation(caption = str_wrap(paste(
    "Migration position score = (restrictive - liberal) / (restrictive + liberal), scheme B, filtered_000,",
    "native tau, code 305 included, speech channel weighted by seat share.",
    "The dispersion measure is mean-invariant, so the two panels answer different questions:",
    "whether the parties moved apart, and whether the chamber as a whole moved.",
    "Both panels run on the same analysis base."), 160))

ggsave(file.path(PATHS$fig_dir, "migration_trajectories.pdf"), p,
       width = 12, height = 5.5, device = cairo_pdf)
ggsave(file.path(PATHS$fig_dir, "migration_trajectories.png"), p,
       width = 12, height = 5.5, dpi = 200, bg = "white")

# ============================================================================
# Console summary
# ============================================================================
cat("\n============== MIGRATION - speech position per party ==============\n")
print(traj %>% select(party, lp, position) %>%
        pivot_wider(names_from = lp, values_from = position) %>%
        mutate(across(where(is.numeric), ~ round(.x, 3))), n = 10, width = Inf)

cat("\n============== SYSTEM level vs spread (speech, canonical) ==============\n")
print(sys_mean %>%
        left_join(sys_disp %>% filter(set == "Excl. AfD") %>% select(lp, disp_ex = dispersion), by = "lp") %>%
        transmute(lp, n_parties, mean_pos = round(mean_pos, 3),
                  dispersion = round(dispersion, 3), dispersion_exAfD = round(disp_ex, 3)),
      n = 20, width = Inf)

first_lp <- min(sys_mean$lp); last_lp <- max(sys_mean$lp)
g <- function(d, col, lp) d[[col]][d$lp == lp]
cat(sprintf("\n  dispersion  %d -> %d : %.3f -> %.3f   (excl. AfD -> %.3f)\n",
            first_lp, last_lp, g(sys_mean, "dispersion", first_lp), g(sys_mean, "dispersion", last_lp),
            g(sys_disp[sys_disp$set == "Excl. AfD", ], "dispersion", last_lp)))
cat(sprintf("  mean        %d -> %d : %+.3f -> %+.3f   (excl. AfD -> %+.3f)\n",
            first_lp, last_lp, g(sys_mean, "mean_pos", first_lp), g(sys_mean, "mean_pos", last_lp),
            g(mean_ex, "mean_pos", last_lp)))
cat("  Reading: spread up AND level not moving towards the restrictive pole = the parties moved APART.\n")

# ---- cell-basis reconciliation (regression test for the 31.07.2026 fix) -----
# The 38-cell table (panel A) and the canonical H3/H2c series (panel B) must now
# agree on the party count in every LP, and the dispersion re-derived from the
# cell table must reproduce the canonical one. Both are printed; PASS/FAIL below.
cat("\n============== CELL-BASIS CHECK (H2b table vs canonical series) ==============\n")
recomputed <- traj %>% group_by(lp) %>%
  summarise(out = list(dalton_polarization(position, seat_share)), .groups = "drop") %>%
  unnest(out) %>% select(lp, n_cells = n_parties, disp_cells = polarization)
cmp <- sys_mean %>% select(lp, n_canonical = n_parties, disp_canonical = dispersion) %>%
  left_join(recomputed, by = "lp") %>%
  mutate(delta_disp = disp_cells - disp_canonical)
print(cmp %>% mutate(across(where(is.double), ~ round(.x, 6))), n = 20, width = Inf)

n_ok    <- all(cmp$n_cells == cmp$n_canonical, na.rm = TRUE)
disp_ok <- all(abs(cmp$delta_disp) < 1e-9, na.rm = TRUE)
cat(sprintf("\n  party counts identical : %s\n  dispersion identical   : %s (max |delta| = %.2e)\n  ==> %s\n",
            if (n_ok) "yes" else "NO",
            if (disp_ok) "yes" else "NO",
            max(abs(cmp$delta_disp), na.rm = TRUE),
            if (n_ok && disp_ok) "PASS - both panels run on the same analysis base"
            else "FAIL - the party-continuity exclusion has been lost upstream (see 15_H2.R block 0b / 16_H3.R block 1b)"))
flag(n_ok && disp_ok,
     "cell-basis reconciliation FAILED - do not publish the figure until it passes")

message("\n[19] Done. Outputs in ", PATHS$out_dir, " and ", PATHS$fig_dir)
