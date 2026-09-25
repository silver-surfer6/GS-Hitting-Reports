# =============================================================================
#  Georgia Southern Baseball | Post Game Hitter Report
#  TrackMan game CSV  ->  1 page letter PDF per batter. Every pitch is mapped
#  against the 2027 NCAA ABS static strike zone with a true size 3.02" ball,
#  the same standard as the umpire and catcher reports.
#
#  Files needed in the working folder:
#    game.csv         TrackMan game export
#    gs_logo.png      interlocking GS, top left
#    sbc_logo.png     Sun Belt mark, top right
#    eagle_logo.png   eagle head, center field on the spray chart
#    hitter_rhh.png   RHH silhouette, transparent background (mirrored for LHH)
# =============================================================================

# ---- 1. MANUAL INPUTS ------------------------------------------------------
batter_name  <- ""                  # blank = every batter our side used,
                                    # or name one exactly as in the CSV ("Estep, Brice")
csv_path     <- "sample_game.csv"   # TrackMan game export
gs_logo      <- "gs_logo.png"      # interlocking GS, top left
sbc_logo     <- "sbc_logo.png"     # Sun Belt, top right
eagle_logo   <- "eagle_logo.png"   # eagle head, center field on the spray chart
hitter_img   <- "hitter_rhh.png"
our_team     <- "GEO_EAG"           # TrackMan code for Georgia Southern
out_dir      <- "."
combined_pdf <- TRUE                # also write every batter into one file

zone_radius_vertical <- TRUE        # count the ball radius on the top/bottom too
MID_HALF     <- 7                   # the heart: middle 14" of the plate, 7" either side
                                    # of center, running the full height of the zone.
                                    # Same touching rule as the zone: any part of the
                                    # 3.02" ball on the band counts (center within 8.51").
hard_hit_ev  <- 95                  # mph
sweet_lo     <- 10; sweet_hi <- 30  # launch angle window that prints green
FREES_GOOD   <- 1                   # walks plus HBP that turn the Frees cell green (any free)
# Color tiers for the headline EV and LA numbers: green, then stepping down to red.
TIER_COL <- c("#1F7A43", "#7FA83F", "#C9A227", "#E07B28", "#C0392B")
EV_TIERS <- c(95, 90, 85, 80)       # 95+ green, 90-95, 85-90, 80-85, under 80 red
LA_STEP  <- 5                       # each 5 degrees outside 10-30, one tier down
# A low confidence / estimated reading that is also physically implausible is
# dropped entirely (EV, LA, distance and spin blank), e.g. 123 mph at -20.
BOGUS_EV_MAX   <- 112               # no doubtful reading above this is kept
BOGUS_CHOP_LA  <- -10               # doubtful and this low ...
BOGUS_CHOP_EV  <- 100               # ... and this hard is not believed either
MAX_SLOTS    <- 7                   # 6 plate appearances plus an emergency 7th
MAX_BIP_ROWS <- 6
# Exit velo readings TrackMan flags at these confidence levels are shown with a
# * and left out of the averages and the hard hit count.
EV_DOUBT     <- c("Low", "Estimated")
# Spin type wording. Intensity is judged on the rpm of the dominant component.
SPIN_HEAVY   <- 3000
SPIN_LIGHT   <- 1200
SPIN_PURE    <- 15                  # degrees off a pure axis still called "pure"
SPIN_SLIGHT  <- 30                  # up to here the second component is "slight"

# Spray chart wall: J.I. Clements Stadium, from the posted dimensions. Distance in
# feet at each spray angle (negative = left field), with the sign posted there.
WALL_PTS <- data.frame(
  ang  = c(-45, -30, -15,   0,  15,  30,  45),
  dist = c(330, 352, 375, 385, 370, 345, 325),
  lab  = c("330'", "352'", "375'", "385'", "370'", "345'", "325'"))
STADIUM_NAME <- "J.I. Clements Stadium"

HEART_LABEL <- "Heart"                # the middle 14" band

# ---- 2. PACKAGES -----------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(ggplot2); library(grid); library(png)
})

# ---- 3. 2027 NCAA ABS STATIC ZONE (inches, ball CENTER as TrackMan reports) -
# Same numbers as umpire_report.R and catcher_report.R. Change all three together.
BALL_R    <- 1.51                      # 3.02" ball
ZONE_HW   <- 9.50                      # 19" wide
TOP_PCT   <- 0.56
BOT_PCT   <- 0.24
TRUE_HW   <- ZONE_HW + BALL_R          # 11.01" ball center
PLATE_HW  <- 8.50
REF_HEIGHT <- 72                       # the NCAA static example: a 6'0" batter
REF_TOP   <- REF_HEIGHT * TOP_PCT      # 40.32"
REF_BOT   <- REF_HEIGHT * BOT_PCT      # 17.28"
TOP_LIM   <- REF_TOP + if (zone_radius_vertical) BALL_R else 0
BOT_LIM   <- REF_BOT - if (zone_radius_vertical) BALL_R else 0
# Drawn lines sit one ball radius inside the center limits, so a drawn ball
# touching a drawn line is exactly a center sitting on the rule line.
DRAW_HW  <- TRUE_HW - BALL_R
DRAW_BOT <- BOT_LIM + BALL_R
DRAW_TOP <- TOP_LIM - BALL_R

# Plate view window (field inches). It opens sideways to fill a wide panel.
# A pitch whose center is more than UNCOMP inches beyond the zone (any side) is
# uncompetitive: it is drawn in a gutter at the very edge of the view, never at its
# true spot. The view is sized so the farthest competitive pitch still sits at
# least UNCOMP_GAP inches (edge to edge) inside that gutter.
UNCOMP     <- 6
UNCOMP_GAP   <- 4      # above the zone
UNCOMP_GAP_X <- 2.5    # beside the zone
ZX_GUTTER  <- TRUE_HW + UNCOMP + UNCOMP_GAP_X + 2 * BALL_R + 0.4 # about 22.9"
ZHI_GUTTER <- TOP_LIM + UNCOMP + UNCOMP_GAP + 2 * BALL_R + 0.4   # about 55.3"
# with no uncompetitive pitch on that side, the view only needs to hold the
# farthest competitive pitch
ZX_TIGHT   <- TRUE_HW + UNCOMP + BALL_R + 0.6                    # about 19.1"
ZHI_TIGHT  <- TOP_LIM + UNCOMP + BALL_R + 0.6                    # about 49.9"
ZXL <- ZX_TIGHT; ZXR <- ZX_TIGHT; ZHI <- ZHI_TIGHT               # set per panel
ZLO <- -3.4                                                       # ground, so the plate shows
# size the view for one plate appearance: open a gutter only where it is needed
set_view <- function(p) {
  q <- p[p$has_loc, ]
  # a gutter only on the side (3B = left, 1B = right) that needs one
  ZXL <<- if (any(q$x < -(TRUE_HW + UNCOMP))) ZX_GUTTER else ZX_TIGHT
  ZXR <<- if (any(q$x >  (TRUE_HW + UNCOMP))) ZX_GUTTER else ZX_TIGHT
  ZHI <<- if (any(q$z > TOP_LIM + UNCOMP)) ZHI_GUTTER else ZHI_TIGHT
}

# ---- 4. BRAND ----------------------------------------------------------------
GARNET <- "#041E42"   # Georgia Southern navy (the accent color throughout)
GOLD   <- "#8B734B"; SILVER <- "#A1A8AD"
BLACK  <- "#121212"; GREY <- "#6B6B6B"
LIGHT  <- "#EDEDED"; RULE  <- "#D9D9D9"
GOOD   <- "#1F6F43"; BAD <- GARNET; NEUTRAL <- "#8A8A8A"
DIRT   <- "#C8A57B"; GRASS <- "#6B8F5A"; GRASS_IN <- "#7FA06D"; CHALK <- "white"
MID_COL <- "#3F3F3F"
HEART_FILL <- "#E8F5E4"             # very light green inside the heart               # the heart (middle 14") dotted lines
FONT   <- "Helvetica"

# Pitch type colors, dark enough to carry a white number
# Pitch type colors, matched to our TrackMan pitch type legend
PITCH_COL <- c(FB = "#4064C4", SI = "#E4E448", CT = "#F09C38", SL = "#74A830",
               SW = "#CC5074", CB = "#C84428", CH = "#8C1894", SPL = "#50A898",
               KN = "#8A8A8A", OTH = "#8A8A8A")
# light fills (sinker, cutter) carry a dark number; hollow balls use a darker ring
lum <- function(hex) { v <- col2rgb(hex) / 255; as.vector(0.299 * v[1, ] + 0.587 * v[2, ] + 0.114 * v[3, ]) }
num_col  <- function(fill) ifelse(lum(fill) > 0.62, BLACK, "white")
ring_col <- function(fill) ifelse(lum(fill) > 0.62,
  vapply(fill, function(h) { v <- col2rgb(h) * 0.72; rgb(v[1], v[2], v[3], maxColorValue = 255) }, ""),
  fill)
PITCH_NAME <- c(FB = "Fastball (4S)", SI = "Sinker", CT = "Cutter", SL = "Slider",
                SW = "Sweeper", CB = "Curveball", CH = "Change", SPL = "Splitter",
                KN = "Knuckleball", OTH = "Other")

team_names <- c(
  GEO_EAG = "Georgia Southern", GAS_EAG = "Georgia Southern",
  SOU_GAM = "South Carolina", COA_CHA = "Coastal Carolina", CIT_BUL = "The Citadel",
  APP_MOU = "Appalachian State", ARK_STA = "Arkansas State", GEO_STA = "Georgia State",
  JAM_MAD = "James Madison", LOU_RAG = "Louisiana", LOU_CAR = "Louisiana Monroe",
  MAR_THU = "Marshall", OLD_DOM = "Old Dominion", SOU_ALA = "South Alabama",
  SOU_MIS = "Southern Miss", TEX_STA = "Texas State", TRO_TRO = "Troy",
  CLE_TIG = "Clemson", ALA_CRI = "Alabama", ARK_RAZ = "Arkansas", AUB_TIG = "Auburn",
  FLA_GAT = "Florida", GEO_BUL = "Georgia", KEN_WIL = "Kentucky", LSU_TIG = "LSU",
  MIS_REB = "Ole Miss", MSU_BDG = "Mississippi State", MIZ_TIG = "Missouri",
  OKL_SOO = "Oklahoma", TEN_VOL = "Tennessee", TEX_LON = "Texas", TEX_AGG = "Texas A&M",
  VAN_COM = "Vanderbilt")
tname <- function(code) ifelse(code %in% names(team_names), team_names[code], code)

pitch_abbr <- c(Fastball = "FB", FourSeamFastBall = "FB", Four_Seam = "FB",
  TwoSeamFastBall = "SI", Sinker = "SI", Cutter = "CT", Slider = "SL", Sweeper = "SW",
  Curveball = "CB", KnuckleCurve = "CB", ChangeUp = "CH", Changeup = "CH",
  Splitter = "SPL", Knuckleball = "KN")

last_name <- function(s) vapply(strsplit(s, ",\\s*"), function(p) p[1], "")
disp_name <- function(s)
  vapply(strsplit(s, ",\\s*"), function(p) if (length(p) == 2) paste(p[2], p[1]) else p[1], "")
ordinal <- function(n) paste0(n, ifelse(n %% 100 %in% 11:13, "th",
                              c("th", "st", "nd", "rd", rep("th", 6))[n %% 10 + 1]))

# ---- 5. LOAD -----------------------------------------------------------------
raw <- read_csv(csv_path, show_col_types = FALSE)
home <- raw$HomeTeam[1]; away <- raw$AwayTeam[1]
game_date <- as.Date(raw$Date[1])
stadium   <- gsub("([a-z])([A-Z])", "\\1 \\2", raw$Stadium[1])
my_team <- if (nzchar(batter_name) && batter_name %in% raw$Batter) {
  raw$BatterTeam[raw$Batter == batter_name][1]
} else if (our_team %in% c(home, away)) our_team else home

SWINGS <- c("StrikeSwinging", "FoulBall", "FoulBallFieldable", "FoulBallNotFieldable",
            "FoulTip", "InPlay")
HITS   <- c("Single", "Double", "Triple", "HomeRun")

raw <- raw %>% arrange(PitchNo) %>% mutate(
  ptype = ifelse(is.na(TaggedPitchType) | TaggedPitchType %in% c("Undefined", "Other"),
                 AutoPitchType, TaggedPitchType),
  pabbr = ifelse(ptype %in% names(pitch_abbr), pitch_abbr[ptype], "OTH"),
  pcol  = PITCH_COL[pabbr],
  half  = ifelse(`Top/Bottom` == "Top", "Top", "Bot"),
  # TrackMan PlateLocSide is + toward 3B. Umpire's view: + = 1B side.
  x = -PlateLocSide * 12, z = PlateLocHeight * 12,
  has_loc  = !is.na(x) & !is.na(z),
  in_zone  = has_loc & abs(x) <= TRUE_HW & z >= BOT_LIM & z <= TOP_LIM,
  # heart: the ball touches the middle 14" band anywhere in the zone's height
  mid14    = in_zone & abs(x) <= MID_HALF + BALL_R,
  swing    = PitchCall %in% SWINGS,
  whiff    = PitchCall == "StrikeSwinging",
  chase    = swing & has_loc & !in_zone,
  iz_whiff = whiff & in_zone,
  bip      = PitchCall == "InPlay",
  ev_doubt = HitLaunchConfidence %in% EV_DOUBT,
  ev_bogus = ev_doubt & !is.na(ExitSpeed) &
             (ExitSpeed > BOGUS_EV_MAX |
              (!is.na(Angle) & Angle < BOGUS_CHOP_LA & ExitSpeed > BOGUS_CHOP_EV) |
              (!is.na(Angle) & abs(Angle) > 80)),
  ExitSpeed   = ifelse(ev_bogus, NA, ExitSpeed),
  Angle       = ifelse(ev_bogus, NA, Angle),
  Distance    = ifelse(ev_bogus, NA, Distance),
  Direction   = ifelse(ev_bogus, NA, Direction),
  HitSpinRate = ifelse(ev_bogus, NA, HitSpinRate),
  HitSpinAxis = ifelse(ev_bogus, NA, HitSpinAxis),
  ev_ok    = !is.na(ExitSpeed) & !ev_doubt,
  # a plate appearance ends on a K, a BB, a ball in play or a hit by pitch. Anything
  # else (a caught stealing, a pickoff) leaves the batter with an incomplete PA.
  terminal = KorBB %in% c("Strikeout", "Walk") | PitchCall %in% c("InPlay", "HitByPitch"))

n_bogus <- sum(raw$ev_bogus)
if (n_bogus) message(sprintf("Dropped %d implausible low confidence batted ball reading%s.",
                             n_bogus, ifelse(n_bogus == 1, "", "s")))

ev_tier <- function(ev) {
  if (is.na(ev)) return(GREY)
  TIER_COL[sum(ev < EV_TIERS) + 1]
}
la_tier <- function(la) {
  if (is.na(la)) return(GREY)
  off <- max(0, sweet_lo - la, la - sweet_hi)
  TIER_COL[min(length(TIER_COL), ceiling(off / LA_STEP) + 1)]
}

# ---- 6. SPIN TYPE ------------------------------------------------------------
# HitSpinAxis uses the pitch convention: 180 = pure backspin, 0/360 = pure
# topspin, 90 = pure RH slider spin (ball breaks to the left looking out from
# the plate), 270 = pure LH slider spin (breaks right). Verified on the sample
# game: fly balls and popups average about 180, grounders sit near 0/360, RHH
# pulled liners carry 100-130 (hook) and RHH oppo flies 200-225 (slice).
cap1 <- function(s) paste0(toupper(substr(s, 1, 1)), substring(s, 2))
spin_type <- function(axis, rpm) {
  out <- rep("--", length(axis))
  ok <- !is.na(axis) & !is.na(rpm) & rpm > 0
  if (!any(ok)) return(out)
  a <- (axis[ok] %% 360) * pi / 180
  back <- -cos(a); side <- sin(a)
  pb <- abs(back) >= abs(side)
  theta <- atan2(pmin(abs(back), abs(side)), pmax(abs(back), abs(side))) * 180 / pi
  prim <- ifelse(pb, ifelse(back > 0, "backspin", "topspin"),
                     ifelse(side > 0, "RH SL spin", "LH SL spin"))
  sec  <- ifelse(pb, ifelse(side > 0, "RH SL spin", "LH SL spin"),
                     ifelse(back > 0, "backspin", "topspin"))
  comp <- rpm[ok] * pmax(abs(back), abs(side))
  int  <- ifelse(comp >= SPIN_HEAVY, "heavy ", ifelse(comp < SPIN_LIGHT, "light ", ""))
  out[ok] <- ifelse(theta < SPIN_PURE,
                    paste0("Pure ", prim, ifelse(nzchar(int), paste0("\n", trimws(int)), "")),
             ifelse(theta < SPIN_SLIGHT, paste0(cap1(paste0(int, prim)), "\nslight ", sec),
                                         paste0(cap1(paste0(int, prim)), "\n+ ", sec)))
  out
}

# ---- 7. RESULT WORDING -------------------------------------------------------
out_word <- c(GroundBall = "Groundout", FlyBall = "Flyout", LineDrive = "Lineout",
              Popup = "Popout", Bunt = "Bunt out")
pa_result <- function(r) {
  pr <- r$PlayResult; ht <- r$TaggedHitType
  if (r$KorBB == "Strikeout")
    return(if (r$PitchCall == "StrikeCalled") "Strikeout looking" else "Strikeout swinging")
  if (r$KorBB == "Walk") return("Walk")
  if (r$PitchCall == "HitByPitch") return("Hit by pitch")
  switch(pr,
    Single = "Single", Double = "Double", Triple = "Triple", HomeRun = "Home run",
    Error = "Reached on error", FieldersChoice = "Fielder's choice",
    Sacrifice = if (ht == "Bunt") "Sac bunt" else "Sac fly",
    Out = if (ht %in% names(out_word)) out_word[[ht]] else "Out",
    "In play")
}
short_result <- c(Single = "1B", Double = "2B", Triple = "3B", HomeRun = "HR",
                  Error = "ROE", FieldersChoice = "FC", Sacrifice = "SAC", Out = "Out")
pitch_word <- function(pc, korbb, pr, ev, ev_ok, la = NA) {
  w <- switch(pc, StrikeCalled = "Called strike", BallCalled = "Ball", BallinDirt = "Ball",
              BallIntentional = "Int. ball", StrikeSwinging = "Whiff", FoulTip = "Foul tip",
              FoulBall = "Foul", FoulBallFieldable = "Foul", FoulBallNotFieldable = "Foul",
              HitByPitch = "HBP", InPlay = "In play", pc)
  if (korbb == "Strikeout") w <- paste(w, "- K")
  if (korbb == "Walk") w <- paste(w, "- BB")
  if (pc == "InPlay") {
    w <- if (pr %in% names(short_result)) short_result[[pr]] else "In play"
    if (!is.na(ev)) w <- sprintf("%s, %.0f EV%s%s", w, ev, if (ev_ok) "" else "*",
                                 if (is.na(la)) "" else sprintf("/%.0f LA", la))
  }
  w
}
reached <- function(res) res %in% c("Single", "Double", "Triple", "Home run", "Walk",
                                    "Hit by pitch", "Reached on error")

# ---- 8. PLOTS ----------------------------------------------------------------
circle_df <- function(x, y, r, n = 44) {
  if (!length(x)) return(data.frame(x = numeric(), y = numeric(), id = integer()))
  th <- seq(0, 2 * pi, length.out = n)
  do.call(rbind, lapply(seq_along(x), function(i)
    data.frame(x = x[i] + r * cos(th), y = y[i] + r * sin(th), id = i)))
}
# nudge number labels apart when balls overlap
spread_labels <- function(x, y, r, iters = 1) {
  lx <- x; ly <- y; n <- length(x)
  if (n > 1) for (it in seq_len(iters)) for (i in 1:(n - 1)) for (j in (i + 1):n) {
    dx <- lx[j] - lx[i]; dy <- ly[j] - ly[i]; dd <- sqrt(dx^2 + dy^2)
    if (!is.na(dd) && is.finite(dd) && dd < 2 * r) {
      if (dd < 1e-6) { dx <- r; dy <- 0; dd <- r }
      sh <- min(0.45 * r, (2 * r - dd) / 2 + 0.2 * r)
      lx[i] <- lx[i] - sh * dx / dd; ly[i] <- ly[i] - sh * dy / dd
      lx[j] <- lx[j] + sh * dx / dd; ly[j] <- ly[j] + sh * dy / dd
    }
  }
  list(x = lx, y = ly)
}

# hitter shadow, same image and geometry as the umpire report
HIT_RHH <- readPNG(hitter_img)
HIT_LHH <- HIT_RHH[, rev(seq_len(dim(HIT_RHH)[2])), ]
HIT_HEAD_PX <- 88; HIT_FOOT_PX <- 440
HIT_STANCE  <- 66; HIT_FRONT <- 11.5; HIT_FADE <- 0.40
hitter_layer <- function(hand) {
  ipp <- HIT_STANCE / (HIT_FOOT_PX - HIT_HEAD_PX)
  hpx <- dim(HIT_RHH)[1]; wpx <- dim(HIT_RHH)[2]
  bot <- -(hpx - HIT_FOOT_PX) * ipp; top <- bot + hpx * ipp; w <- wpx * ipp
  img <- if (hand == "R") HIT_RHH else HIT_LHH
  img[, , 1:3] <- 1 - (1 - img[, , 1:3]) * HIT_FADE
  if (hand == "R") { x1 <- -HIT_FRONT; x0 <- x1 - w } else { x0 <- HIT_FRONT; x1 <- x0 + w }
  annotation_raster(img, xmin = x0, xmax = x1, ymin = bot, ymax = top, interpolate = TRUE)
}

# One plate appearance at the plate, umpire's view, zone and ball to scale.
# Balls outside the window are pinned to its edge and drawn hollow.
pa_zone_plot <- function(d, hand, scale, xlim, ylo = ZLO, yhi = ZHI) {
  if (length(xlim) == 1) xlim <- c(-xlim, xlim)   # c(left edge, right edge), field inches
  lw   <- 0.017 / scale                    # zone line, drawn inward only
  dash <- 0.006 / scale                    # middle 14" line weight
  plate <- data.frame(x = c(-PLATE_HW, PLATE_HW, PLATE_HW, 0, -PLATE_HW),
                      y = c(0, 0, -1.3, -2.6, -1.3))
  p <- ggplot() + hitter_layer(hand) +
    annotate("rect", xmin = -DRAW_HW, xmax = DRAW_HW, ymin = DRAW_BOT, ymax = DRAW_TOP,
             fill = "white", colour = NA) +
    annotate("rect", xmin = -MID_HALF, xmax = MID_HALF, ymin = DRAW_BOT, ymax = DRAW_TOP,
             fill = HEART_FILL, colour = NA) +
    geom_polygon(data = plate, aes(x, y), fill = "white", colour = "#9A9A9A", linewidth = 0.4) +
    annotate("rect", xmin = -DRAW_HW, xmax = -DRAW_HW + lw, ymin = DRAW_BOT, ymax = DRAW_TOP, fill = BLACK, colour = NA) +
    annotate("rect", xmin = DRAW_HW - lw, xmax = DRAW_HW, ymin = DRAW_BOT, ymax = DRAW_TOP, fill = BLACK, colour = NA) +
    annotate("rect", xmin = -DRAW_HW, xmax = DRAW_HW, ymin = DRAW_BOT, ymax = DRAW_BOT + lw, fill = BLACK, colour = NA) +
    annotate("rect", xmin = -DRAW_HW, xmax = DRAW_HW, ymin = DRAW_TOP - lw, ymax = DRAW_TOP, fill = BLACK, colour = NA) +
    # middle 14": dotted lines exactly 7" either side of center, under the balls
    annotate("segment", x = c(-MID_HALF, MID_HALF), xend = c(-MID_HALF, MID_HALF),
             y = DRAW_BOT + lw, yend = DRAW_TOP - lw, colour = MID_COL,
             linewidth = 0.65, linetype = "22")
  d <- d %>% filter(has_loc)
  if (nrow(d)) {
    m <- BALL_R + 0.4
    far_x  <- abs(d$x) > TRUE_HW + UNCOMP
    far_hi <- d$z > TOP_LIM + UNCOMP
    far_lo <- d$z < BOT_LIM - UNCOMP
    off <- far_x | far_hi | far_lo                  # uncompetitive
    # competitive pitches at their true spot; uncompetitive ones in the edge gutter
    px <- ifelse(far_x, ifelse(d$x < 0, xlim[1] + m, xlim[2] - m),
                 pmin(pmax(d$x, xlim[1] + m), xlim[2] - m))
    pz <- ifelse(far_hi, yhi - m, ifelse(far_lo, ylo + m, pmin(pmax(d$z, ylo + m), yhi - m)))
    lab <- spread_labels(px, pz, BALL_R)
    sw <- d$swing                                   # swing = solid, take = hollow
    cd <- circle_df(px, pz, BALL_R); cd$fc <- d$pcol[cd$id]
    fs_pt <- min(8.5, max(3.2, 2 * BALL_R * scale * 72 * 0.62))
    tl <- data.frame(x = lab$x, y = lab$y, no = d$PitchofPA, sw = sw, fc = d$pcol)
    ring_lw <- max(0.6, min(1.1, BALL_R * scale * 72 * 0.09))
    if (any(off))
      p <- p + geom_path(data = circle_df(px[off], pz[off], BALL_R + 0.75, 60),
                         aes(x, y, group = id), colour = GREY, linewidth = 0.35, linetype = "22")
    p <- p +
      geom_polygon(data = cd[sw[cd$id], ], aes(x, y, group = id, fill = fc), colour = NA) +
      geom_polygon(data = circle_df(px[!sw], pz[!sw], BALL_R - 0.25),
                   aes(x, y, group = id), fill = "white",
                   colour = rep(ring_col(d$pcol[!sw]), each = 44), linewidth = ring_lw) +
      scale_fill_identity() +
      geom_text(data = tl, aes(x, y, label = no), colour = ifelse(tl$sw, num_col(tl$fc), ring_col(tl$fc)),
                family = FONT, fontface = "bold",
                size = ifelse(tl$no >= 10, fs_pt * 0.85, fs_pt) / .pt)
  }
  p + coord_fixed(xlim = xlim, ylim = c(ylo, yhi), expand = FALSE) +
    theme_void() + theme(plot.margin = margin(0, 0, 0, 0))
}

res_fill <- function(pr) ifelse(pr %in% HITS, GOOD, ifelse(pr == "Out", BAD, NEUTRAL))

# Spray chart, to scale in feet, looking out from home plate (LF on the left)
# distance from home to the wall along a spray angle, smoothed through the posts
wall_dist <- function(ang)
  pmax(0, stats::spline(WALL_PTS$ang, WALL_PTS$dist, xout = pmin(pmax(ang, -45), 45),
                        method = "natural")$y)
WALL_ANG <- seq(-45, 45, by = 0.5)
wall_xy <- function() {
  d <- wall_dist(WALL_ANG)
  data.frame(x = d * sin(WALL_ANG * pi / 180), y = d * cos(WALL_ANG * pi / 180))
}
spray_frame <- function() {
  w <- wall_xy()
  list(xlim = range(w$x) + c(-44, 44), ylim = c(-12, max(w$y) + 30))
}
spray_plot <- function(b, marker_r, lab_pt = 6.5) {
  f <- spray_frame()
  wall <- wall_xy()
  field <- rbind(data.frame(x = 0, y = 0), wall)
  # infield dirt: 95 ft arc around the rubber, clipped to fair ground
  th <- seq(0, 2 * pi, length.out = 240)
  arc <- data.frame(x = 95 * cos(th), y = 60.5 + 95 * sin(th))
  arc <- arc[abs(atan2(arc$x, arc$y)) <= pi / 4 & arc$y > 0, ]
  arc <- arc[order(atan2(arc$x, arc$y)), ]
  dirt <- rbind(data.frame(x = 0, y = 0), arc)
  s <- 90 / sqrt(2)
  grass_in <- data.frame(x = c(0, s, 0, -s) * 0.86, y = c(9, s, 2 * s - 6, s))
  bases <- data.frame(x = c(s, 0, -s), y = c(s, 2 * s, s))
  signs <- WALL_PTS
  signs$d <- signs$dist + 20
  signs$x <- signs$d * sin(signs$ang * pi / 180)
  signs$y <- signs$d * cos(signs$ang * pi / 180)
  signs$y[signs$ang == 0] <- max(wall$y) + 16
  # corner signs sit beside the pole rather than past it
  pole <- abs(signs$ang) == 45
  signs$x[pole] <- sign(signs$ang[pole]) * (max(wall$x) + 16)
  signs$y[pole] <- wall$y[1] - 20
  p <- ggplot() +
    geom_polygon(data = field, aes(x, y), fill = GRASS, colour = NA) +
    geom_polygon(data = dirt, aes(x, y), fill = DIRT, colour = NA) +
    geom_polygon(data = grass_in, aes(x, y), fill = GRASS_IN, colour = NA) +
    annotate("point", x = 0, y = 60.5, size = 1.1, colour = DIRT) +
    geom_point(data = bases, aes(x, y), shape = 22, size = 1.0, fill = "white", colour = "white") +
    annotate("segment", x = 0, y = 0,
             xend = c(wall$x[1], wall$x[nrow(wall)]), yend = c(wall$y[1], wall$y[nrow(wall)]),
             colour = CHALK, linewidth = 0.45) +
    annotation_raster(EAGLE, xmin = -EAGLE_W / 2, xmax = EAGLE_W / 2,
                      ymin = EAGLE_Y, ymax = EAGLE_Y + EAGLE_W * EAGLE_AR,
                      interpolate = TRUE) +
    annotate("path", x = wall$x, y = wall$y, colour = BLACK, linewidth = 1.3) +
    geom_text(data = signs, aes(x, y, label = lab), family = FONT, fontface = "bold",
              size = lab_pt / .pt, colour = BLACK)
  k <- b %>% filter(!is.na(Distance), !is.na(Direction))
  if (nrow(k)) {
    k$px <- k$Distance * sin(k$Direction * pi / 180)
    k$py <- k$Distance * cos(k$Direction * pi / 180)
    m <- marker_r * 1.1
    k$cx <- pmin(pmax(k$px, f$xlim[1] + m), f$xlim[2] - m)
    k$cy <- pmin(pmax(k$py, f$ylim[1] + m), f$ylim[2] - m)
    lab <- spread_labels(k$cx, k$cy, marker_r, 30); k$cx <- lab$x; k$cy <- lab$y
    cd <- circle_df(k$cx, k$cy, marker_r); cd$fc <- k$fc[cd$id]
    p <- p + geom_polygon(data = cd, aes(x, y, group = id, fill = fc),
                          colour = "white", linewidth = 0.4) +
      scale_fill_identity() +
      geom_text(data = k, aes(cx, cy, label = pa_no), colour = "white",
                family = FONT, fontface = "bold", size = 7.4 / .pt)
  }
  p + coord_fixed(xlim = f$xlim, ylim = f$ylim, expand = FALSE) +
    theme_void() + theme(plot.margin = margin(0, 0, 0, 0))
}

# Contact point, overhead and to scale (feet), umpire's view: the pitcher is at
# the top, the first base side on the right.
PLATE_DEPTH <- 17 / 12; PLATE_SIDE <- 8.5 / 12
BOX_W <- 4; BOX_L <- 6; BOX_GAP <- 6 / 12; CHALK_W <- 2.5 / 12
CT_FRAME <- list(hw = 2.6, ylo = -1.0, yhi = 4.3)
chalk_rect <- function(x0, x1, y0, y1, t = CHALK_W, id = "") {
  parts <- list(
    data.frame(x = c(x0, x0 + t, x0 + t, x0), y = c(y0, y0, y1, y1)),
    data.frame(x = c(x1 - t, x1, x1, x1 - t), y = c(y0, y0, y1, y1)),
    data.frame(x = c(x0, x1, x1, x0), y = c(y0, y0, y0 + t, y0 + t)),
    data.frame(x = c(x0, x1, x1, x0), y = c(y1 - t, y1 - t, y1, y1)))
  do.call(rbind, parts) %>% mutate(g = paste0(id, rep(seq_along(parts), each = 4)))
}
contact_plot <- function(b, hands, marker_r, f = CT_FRAME) {
  ctr <- PLATE_DEPTH / 2
  bin <- PLATE_HW / 12 + BOX_GAP; bout <- bin + BOX_W
  plate <- data.frame(
    x = c(-PLATE_HW / 12, -PLATE_HW / 12, 0, PLATE_HW / 12, PLATE_HW / 12),
    y = c(PLATE_DEPTH, PLATE_DEPTH - PLATE_SIDE, 0, PLATE_DEPTH - PLATE_SIDE, PLATE_DEPTH))
  chalk <- rbind(chalk_rect(-bout, -bin, ctr - BOX_L / 2, ctr + BOX_L / 2, id = "L"),
                 chalk_rect(bin, bout, ctr - BOX_L / 2, ctr + BOX_L / 2, id = "R"))
  p <- ggplot() +
    annotate("rect", xmin = -f$hw, xmax = f$hw, ymin = f$ylo, ymax = f$yhi, fill = DIRT, colour = NA) +
    geom_polygon(data = chalk, aes(x, y, group = g), fill = CHALK, colour = NA) +
    # front of the plate, carried across as a faint reference line
    annotate("segment", x = -bin, xend = bin, y = PLATE_DEPTH, yend = PLATE_DEPTH,
             colour = "#E9DCCB", linewidth = 0.3, linetype = "22") +
    geom_polygon(data = plate, aes(x, y), fill = "white", colour = BLACK, linewidth = 0.4)
  for (h in hands)
    p <- p + annotate("text", x = (if (h == "R") -1 else 1) * (bin + f$hw) / 2, y = ctr - 2.0,
                      label = paste0(h, "HH"), colour = "#A3835E", family = FONT,
                      fontface = "bold", size = 7 / .pt)
  k <- b %>% filter(!is.na(ContactPositionX), !is.na(ContactPositionZ))
  if (nrow(k)) {
    k$px <- k$ContactPositionZ; k$py <- k$ContactPositionX
    m <- marker_r * 1.1
    k$cx <- pmin(pmax(k$px, -f$hw + m), f$hw - m)
    k$cy <- pmin(pmax(k$py, f$ylo + m), f$yhi - m)
    k$off <- abs(k$cx - k$px) > 1e-6 | abs(k$cy - k$py) > 1e-6
    lab <- spread_labels(k$cx, k$cy, marker_r, 30); k$cx <- lab$x; k$cy <- lab$y
    cd <- circle_df(k$cx, k$cy, marker_r); cd$fc <- k$fc[cd$id]; cd$sld <- !k$off[cd$id]
    p <- p +
      geom_polygon(data = cd[cd$sld, ], aes(x, y, group = id, fill = fc), colour = "white", linewidth = 0.35) +
      geom_polygon(data = cd[!cd$sld, ], aes(x, y, group = id, colour = fc), fill = "white", linewidth = 0.7) +
      scale_fill_identity() + scale_colour_identity() +
      geom_text(data = k, aes(cx, cy, label = pa_no), colour = ifelse(k$off, k$fc, "white"),
                family = FONT, fontface = "bold", size = 6.2 / .pt)
  }
  p + coord_fixed(xlim = c(-f$hw, f$hw), ylim = c(f$ylo, f$yhi), expand = FALSE) +
    theme_void() + theme(plot.margin = margin(0, 0, 0, 0))
}

# ---- 9. GRID HELPERS ---------------------------------------------------------
IN <- function(v) unit(v, "inches")
txt <- function(label, x, y, size, col = BLACK, face = "plain", just = "centre", ...)
  grid.text(label, x, y, just = just,
            gp = gpar(fontfamily = FONT, fontsize = size, col = col, fontface = face), ...)
tw <- function(label, size, face = "bold")
  convertWidth(grobWidth(textGrob(label, gp = gpar(fontfamily = FONT, fontsize = size,
                                                   fontface = face))), "inches", TRUE)
fit_fs <- function(label, max_w, fs, min_fs = 5, face = "bold") {
  while (fs > min_fs && tw(label, fs, face) > max_w) fs <- fs - 0.25
  fs
}
vp_w <- function() convertWidth(unit(1, "npc"), "inches", TRUE)
vp_h <- function() convertHeight(unit(1, "npc"), "inches", TRUE)
draw_logo <- function(path, x, y, h, just)
  grid.draw(rasterGrob(readPNG(path), x = x, y = y, height = h, just = just, interpolate = TRUE))
LOGO_GS <- readPNG(gs_logo); LOGO_SBC <- readPNG(sbc_logo)
EAGLE    <- readPNG(eagle_logo)
EAGLE_AR <- dim(EAGLE)[1] / dim(EAGLE)[2]
EAGLE_W  <- 150     # feet wide, centered in the outfield grass
EAGLE_Y  <- 215

panel <- function(title, stat = NULL, title_col = BLACK, bar_h = 0.22) {
  grid.rect(gp = gpar(fill = "white", col = RULE, lwd = 0.8))
  pushViewport(viewport(y = 1, height = IN(bar_h), just = "top"))
  grid.rect(gp = gpar(fill = LIGHT, col = NA))
  sw <- if (is.null(stat)) 0 else tw(stat, 6.6) + 0.1
  txt(title, IN(0.08), 0.5, fit_fs(title, vp_w() - 0.16 - sw, 8.2, 5.5), title_col, "bold", just = "left")
  if (!is.null(stat)) txt(stat, unit(1, "npc") - IN(0.08), 0.5, 6.6, GREY, "bold", just = "right")
  popViewport()
  viewport(y = 0, height = unit(1, "npc") - IN(bar_h + 0.02), just = "bottom")
}

# generic table; cells may hold "\n" for two lines. ball_col fills column 1.
table_grid <- function(tbl, widths, aligns, row_h, hdr_h = row_h, fs = 7, ball_col = NULL, hollow = NULL,
                       col_cols = NULL, hdr_fill = BLACK, pad = 0.05, ball_r = NULL) {
  W <- vp_w(); Ht <- vp_h()
  sc <- W / sum(widths); xs <- cumsum(c(0, widths)) * sc
  grid.rect(IN(0), IN(Ht), IN(W), IN(hdr_h), just = c("left", "top"),
            gp = gpar(fill = hdr_fill, col = NA))
  for (j in seq_along(tbl)) {
    xj <- if (aligns[j] == "l") xs[j] + pad else xs[j] + widths[j] * sc / 2
    grid.text(names(tbl)[j], IN(xj), IN(Ht - hdr_h / 2),
              just = if (aligns[j] == "l") "left" else "centre",
              gp = gpar(fontfamily = FONT, fontsize = fs - 0.6, col = "white",
                        fontface = "bold", lineheight = 0.92))
  }
  for (i in seq_len(nrow(tbl))) {
    yt <- Ht - hdr_h - row_h * (i - 1); yc <- yt - row_h / 2
    grid.rect(IN(0), IN(yt), IN(W), IN(row_h), just = c("left", "top"),
              gp = gpar(fill = if (i %% 2 == 0) "#F6F6F6" else "white", col = NA))
    for (j in seq_along(tbl)) {
      v <- as.character(tbl[i, j])
      xj <- if (aligns[j] == "l") xs[j] + pad else xs[j] + widths[j] * sc / 2
      if (j == 1 && !is.null(ball_col)) {
        br <- if (is.null(ball_r)) min(0.075, row_h * 0.40) else ball_r
        log_ball(IN(xj), IN(yc), br, ball_col[i], v, min(fs - 0.5, br * 72 * 1.25),
                 !is.null(hollow) && hollow[i])
        next
      }
      cc <- if (!is.null(col_cols) && !is.na(col_cols[i, j])) col_cols[i, j] else BLACK
      grid.text(v, IN(xj), IN(yc), just = if (aligns[j] == "l") "left" else "centre",
                gp = gpar(fontfamily = FONT, fontsize = fs, col = cc,
                          fontface = if (!is.null(col_cols) && !is.na(col_cols[i, j])) "bold" else "plain",
                          lineheight = 0.92))
    }
  }
}

# ---- 10. ONE BATTER ----------------------------------------------------------
build_batter <- function(bn) {
  me <- raw %>% filter(Batter == bn)

  # appearances: a batter's run of pitches in one PAofInning of one half inning
  app <- me %>%
    group_by(Inning, half, PAofInning) %>%
    summarise(first_pitch = min(PitchNo), n = n(), complete = any(terminal),
              .groups = "drop") %>%
    arrange(first_pitch)
  app$pa_no <- NA_integer_
  app$pa_no[app$complete] <- seq_len(sum(app$complete))
  over <- max(0, nrow(app) - MAX_SLOTS)
  shown <- head(app, MAX_SLOTS)

  me <- me %>% left_join(app %>% select(Inning, half, PAofInning, pa_no, complete),
                         by = c("Inning", "half", "PAofInning"))

  pa_rows <- lapply(seq_len(nrow(shown)), function(i) {
    a <- shown[i, ]
    p <- me %>% filter(Inning == a$Inning, half == a$half, PAofInning == a$PAofInning) %>%
      arrange(PitchNo)
    last <- p[nrow(p), ]
    res <- if (a$complete) pa_result(p[p$terminal, ][1, ]) else "Incomplete"
    list(a = a, p = p, res = res,
         hand = if (p$BatterSide[1] == "Left") "L" else "R",
         vs = sprintf("vs. %sHP %s", substr(p$PitcherThrows[1], 1, 1), last_name(p$Pitcher[1])))
  })

  # incomplete appearance disclaimers
  inc_notes <- unlist(lapply(pa_rows, function(r) {
    if (r$a$complete) return(NULL)
    w <- vapply(seq_len(nrow(r$p)), function(k) tolower(pitch_word(
      r$p$PitchCall[k], "Undefined", "Undefined", NA, TRUE)), "")
    what <- if (length(w) == 1) paste("for a", tolower(w)) else
      paste0("(", paste(w, collapse = ", "), ")")
    sprintf("Incomplete PA seeing %d pitch%s %s in the %s inning against %s. %s",
            nrow(r$p), ifelse(nrow(r$p) == 1, "", "es"), what, ordinal(r$a$Inning),
            sub("^vs\\. ", "", r$vs),
            ifelse(nrow(r$p) == 1, "That pitch counts in the totals but not as a plate appearance.",
                   "Those pitches count in the totals but not as a plate appearance."))
  }))

  # ---- totals (every pitch seen, complete or not) ----
  term <- me %>% filter(terminal, complete)
  PA  <- nrow(term)
  H   <- sum(term$PlayResult %in% HITS)
  XBH <- sum(term$PlayResult %in% c("Double", "Triple", "HomeRun"))
  BB  <- sum(term$KorBB == "Walk"); K <- sum(term$KorBB == "Strikeout")
  HBP <- sum(term$PitchCall == "HitByPitch")
  SAC <- sum(term$PlayResult == "Sacrifice")
  AB  <- PA - BB - HBP - SAC
  bipd <- me %>% filter(bip) %>% arrange(PitchNo)
  evs  <- bipd$ExitSpeed[bipd$ev_ok]
  las  <- bipd$Angle[bipd$ev_ok & !is.na(bipd$Angle)]
  T <- list(
    pitches = nrow(me), swings = sum(me$swing),
    swing_pct = if (nrow(me)) 100 * mean(me$swing) else NA,
    chase = sum(me$chase), ooz = sum(me$has_loc & !me$in_zone),
    whiffs = sum(me$whiff), iz_whiff = sum(me$iz_whiff),
    iz_sw = sum(me$swing & me$in_zone),
    mid_sw = sum(me$swing & me$mid14), mid_n = sum(me$mid14),
    bip = nrow(bipd),
    avg_ev = if (length(evs)) mean(evs) else NA, max_ev = if (length(evs)) max(evs) else NA,
    avg_la = if (length(las)) mean(las) else NA,
    hh = sum(evs >= hard_hit_ev),
    sweet = sum(las >= sweet_lo & las <= sweet_hi), n_la = length(las),
    sweet_all = sum(bipd$Angle >= sweet_lo & bipd$Angle <= sweet_hi, na.rm = TRUE),
    edge_sw = sum(me$swing & me$in_zone & !me$mid14), edge_n = sum(me$in_zone & !me$mid14))
  hands <- sort(unique(ifelse(me$BatterSide == "Left", "L", "R")))
  bats  <- if (length(hands) > 1) "S" else hands

  # ---- batted balls ----
  bipd <- bipd %>% mutate(fc = res_fill(PlayResult))
  bip_over <- max(0, nrow(bipd) - MAX_BIP_ROWS)
  bip_show <- head(bipd, MAX_BIP_ROWS)

  pcs_present <- names(PITCH_COL)[names(PITCH_COL) %in% me$pabbr]

  list(bn = bn, me = me, bip_all = bipd, pa_rows = pa_rows, over = over, inc_notes = inc_notes,
       PA = PA, AB = AB, H = H, XBH = XBH, BB = BB, K = K, HBP = HBP, SAC = SAC,
       T = T, bats = bats, hands = hands, bip = bip_show, bip_over = bip_over,
       pcs = pcs_present)
}

# ---- 11. PAGE ------------------------------------------------------------------
LOGO_H <- 0.62       # logo height, inches (was 0.77)
HDR_H  <- 0.74       # header band height, inches (was 0.84)
draw_header <- function(bn) {
  grid.draw(rasterGrob(LOGO_GS, x = 0, y = 0.5, height = IN(LOGO_H), just = c("left", "centre")))
  grid.draw(rasterGrob(LOGO_SBC, x = 1, y = 0.5, width = IN(LOGO_H * 1.35),
                       just = c("right", "centre")))
  txt("HITTER REPORT", 0.5, 0.92, 8.5, GARNET, "bold")
  txt(disp_name(bn), 0.5, 0.66, 20, BLACK, "bold")
  txt(sprintf("%s  @  %s", tname(away), tname(home)), 0.5, 0.36, 10.5, BLACK, "bold")
  txt(sprintf("%s   |   %s", format(game_date, "%B %d, %Y"), stadium), 0.5, 0.12, 9.5, GREY, "bold")
  grid.lines(c(0, 1), c(0, 0) - 0.10, gp = gpar(col = GARNET, lwd = 2.2))
}

game_line <- function(R) {
  T <- R$T
  grid.rect(gp = gpar(fill = "white", col = RULE, lwd = 0.8))
  bh <- 0.23
  pushViewport(viewport(y = 1, height = IN(bh), just = "top"))
  grid.rect(gp = gpar(fill = GARNET, col = NA))
  txt("GAME LINE", IN(0.09), 0.5, 9.5, "white", "bold", just = "left")
  txt(sprintf("Bats %s   |   %s", R$bats, tname(my_team)), unit(1, "npc") - IN(0.09), 0.5,
      8.2, "white", "bold", just = "right")
  popViewport()
  pushViewport(viewport(y = 0, height = unit(1, "npc") - IN(bh), just = "bottom"))
  lw <- 0
  f1 <- function(v, fmt = "%.1f") if (is.na(v)) "--" else sprintf(fmt, v)
  # process only: no hits or outs up here
  n_bip <- R$T$bip
  # green only promotes the good: never color a negative line
  cells <- list(
    list(v = T$pitches, l = "Pitches Seen"),
    list(v = R$BB + R$HBP, l = "Frees", s = sprintf("%d BB  \u00b7  %d HBP", R$BB, R$HBP),
         good = R$BB + R$HBP >= FREES_GOOD),
    list(v = T$chase, l = "Chases", s = sprintf("of %d out of zone", T$ooz),
         good = T$chase == 0),
    list(v = T$whiffs, l = "Whiffs", s = sprintf("%d in zone", T$iz_whiff),
         good = T$whiffs == 0 || T$iz_whiff == 0),
    list(v = sprintf("%d/%d", T$mid_sw, T$mid_n), l = paste(HEART_LABEL, "Swings"),
         s = "swung / seen in heart", good = T$mid_sw > 0),
    list(v = sprintf("%d/%d", T$hh, n_bip), l = "Hard Hit Balls", good = T$hh > 0,
         s = sprintf("%d+ mph%s", hard_hit_ev, if (is.na(T$max_ev)) "" else sprintf("  \u00b7  max %.1f", T$max_ev))),
    list(v = sprintf("%d/%d", T$sweet_all, n_bip), l = sprintf("%d-%d\u00b0 LA", sweet_lo, sweet_hi),
         s = "of balls in play", good = T$sweet_all > 0))
  n <- length(cells)
  pushViewport(viewport(layout = grid.layout(1, n)))
  for (i in seq_len(n)) {
    pushViewport(viewport(layout.pos.col = i))
    cw <- vp_w() - 0.06
    v <- as.character(cells[[i]]$v)
    fsv <- fit_fs(v, cw, 21)
    good <- isTRUE(cells[[i]]$good)
    if (good && grepl("/", v)) {
      # a rate: the count is green, the "of" stays black
      num <- sub("/.*", "", v); rest <- sub("^[^/]*", "", v)
      x0 <- 0.5 - (tw(num, fsv) + tw(rest, fsv)) / 2 / vp_w()
      txt(num, unit(x0, "npc"), 0.60, fsv, GOOD, "bold", just = "left")
      txt(rest, unit(x0, "npc") + IN(tw(num, fsv)), 0.60, fsv, BLACK, "bold", just = "left")
    } else txt(v, 0.5, 0.60, fsv, if (good) GOOD else BLACK, "bold")
    txt(toupper(cells[[i]]$l), 0.5, 0.29, fit_fs(toupper(cells[[i]]$l), cw, 6.4), GREY, "bold")
    if (!is.null(cells[[i]]$s))
      txt(cells[[i]]$s, 0.5, 0.13, fit_fs(cells[[i]]$s, cw, 5.6, 4, "plain"), GREY)
    popViewport()
    if (i < n) grid.lines(c(i / n, i / n), c(0.14, 0.86), gp = gpar(col = RULE, lwd = 0.6))
  }
  popViewport(2)
}

legend_strip <- function(pcs) {
  Wn <- vp_w(); x <- 0
  for (k in pcs) {
    grid.circle(IN(x + 0.06), 0.5, IN(0.055), gp = gpar(fill = PITCH_COL[[k]], col = NA))
    txt(PITCH_NAME[[k]], IN(x + 0.15), 0.5, 7, BLACK, just = "left")
    x <- x + 0.15 + tw(PITCH_NAME[[k]], 7, "plain") + 0.16
  }
  hl <- paste0(HEART_LABEL, " (middle 14\")")
  grid.rect(IN(x), 0.5, IN(0.2), IN(0.11), just = c("left", "centre"), gp = gpar(fill = HEART_FILL, col = NA))
  grid.lines(IN(c(x, x)), unit(0.5, "npc") + IN(c(-0.055, 0.055)), gp = gpar(col = MID_COL, lwd = 1.6, lty = "11"))
  grid.lines(IN(c(x + 0.2, x + 0.2)), unit(0.5, "npc") + IN(c(-0.055, 0.055)), gp = gpar(col = MID_COL, lwd = 1.6, lty = "11"))
  txt(hl, IN(x + 0.26), 0.5, 7, BLACK, just = "left")
  x <- x + 0.26 + tw(hl, 7, "plain") + 0.16
  grid.circle(IN(x + 0.06), 0.5, IN(0.055), gp = gpar(fill = GREY, col = NA))
  txt("Swing", IN(x + 0.15), 0.5, 7, BLACK, just = "left")
  x <- x + 0.15 + tw("Swing", 7, "plain") + 0.12
  grid.circle(IN(x + 0.06), 0.5, IN(0.048), gp = gpar(fill = "white", col = GREY, lwd = 1.3))
  txt("Take", IN(x + 0.15), 0.5, 7, BLACK, just = "left")
  x <- x + 0.15 + tw("Take", 7, "plain") + 0.12
  grid.circle(IN(x + 0.07), 0.5, IN(0.07), gp = gpar(fill = NA, col = GREY, lwd = 0.8, lty = "22"))
  grid.circle(IN(x + 0.07), 0.5, IN(0.04), gp = gpar(fill = GREY, col = NA))
  txt("Uncompetitive", IN(x + 0.17), 0.5, 7, BLACK, just = "left")
  x <- x + 0.17 + tw("Uncompetitive", 7, "plain") + 0.14
  txt("Chase in navy", IN(x), 0.5, 7, GARNET, "bold", just = "left")
  x <- x + tw("Chase in navy", 7) + 0.2

}

# rows of plate appearance panels for n slots
# 4 appearances is the most common game and is locked to 2 by 2.
slot_rows <- function(n) switch(as.character(n),
  "1" = 1, "2" = 2, "3" = 3, "4" = c(2, 2), "5" = c(3, 2), "6" = c(3, 3), "7" = c(4, 3), 1)

ZONE_ASPECT <- (ZHI - ZLO)              # field inches tall; width is 2 * ZX

# Width the pitch log needs at a given font size
log_widths <- function(tbl, fs) {
  w <- vapply(seq_along(tbl), function(j)
    max(tw(names(tbl)[j], fs - 0.6), vapply(as.character(tbl[[j]]), tw, 0, size = fs,
                                             face = "plain")), 0)
  w[1] <- max(w[1], 0.16)
  w + 0.09
}

# Choose side by side (zone left, log right) or stacked (zone over log),
# whichever gives the bigger zone while keeping the log readable.
pa_layout <- function(w, h, tbl) {
  np <- nrow(tbl); zw_need <- ZXL + ZXR; zh_need <- ZHI - ZLO
  big <- h > 3.2                           # one or two appearances: a larger log
  row_side <- min(if (big) 0.27 else 0.20, (h - 0.06) / (np + 1))
  fs_side  <- min(if (big) 9.5 else 7.4, row_side * if (big) 36 else 44)
  repeat {
    lw <- sum(log_widths(tbl, fs_side)) + 0.08
    if (lw <= (if (big) 0.40 else 0.55) * w || fs_side <= 5.6) break
    fs_side <- fs_side - 0.2
  }
  s_side <- min((w - lw - 0.14) / zw_need, h / zh_need)
  if (row_side < 0.10) s_side <- 0
  # compact: one colored ball per pitch, "FB 93.5" over the result, no header
  # a long at bat wraps the compact log into a second (or third) column
  s_c <- 0; row_c <- 0.3; fs_c <- 7; lw_c <- 1; nc_c <- 1
  for (nc in 1:3) {
    rows_n <- ceiling(np / nc)
    rw <- min(0.30, (h - 0.04) / rows_n)
    if (rw < 0.20) next                   # keep the log at a readable size
    fz <- min(7.2, rw * 26)
    ew <- function(f) 0.25 + max(vapply(paste(tbl$Pitch, tbl$Velo), tw, 0, size = f),
                                 vapply(tbl$Result, tw, 0, size = f - 0.6, face = "plain")) + 0.08
    each <- ew(fz)
    # step the text down (not below 6 pt) before the log eats into the zone
    while (nc * each > 0.40 * w && fz > 6) { fz <- fz - 0.2; each <- ew(fz) }
    sc_try <- min((w - nc * each - 0.14) / zw_need, h / zh_need)
    if (nc * each < 0.72 * w && sc_try > s_c) {
      s_c <- sc_try; row_c <- rw; fs_c <- fz; lw_c <- nc * each; nc_c <- nc
    }
  }
  row_st <- min(0.16, max(0.10, 0.30 * h / (np + 1)))
  lh <- row_st * (np + 1) + 0.05
  s_st <- min((w - 0.06) / zw_need, (h - lh - 0.03) / zh_need)
  # grid: one line per pitch ("SI 93.4  Foul"), wrapped into columns under the zone;
  # its in-play line uses the short form "1B 95/14" (EV/LA)
  tbl <- short_bip(tbl)
  s_g <- 0; G <- NULL
  for (nc in 1:4) {
    rows_n <- ceiling(np / nc)
    rw <- max(0.105, min(0.16, 0.26 * h / rows_n))
    fz <- min(7, rw * 46)
    br <- min(0.06, rw * 0.36)
    gw <- function(f) {
      pw <- max(vapply(paste(tbl$Pitch, tbl$Velo), tw, 0, size = f))
      c(pw, 2 * br + 0.10 + pw + 0.07 +
              max(vapply(tbl$Result, tw, 0, size = f - 0.4, face = "plain")) + 0.06)
    }
    g <- gw(fz)
    while (nc * g[2] > w - 0.08 && fz > 5.2) { fz <- fz - 0.2; g <- gw(fz) }
    pw <- g[1]; each <- g[2]
    if (nc * each > w - 0.08) next
    lhg <- rows_n * rw + 0.06
    sc_try <- min((w - 0.06) / zw_need, (h - lhg - 0.03) / zh_need)
    if (sc_try > s_g) { s_g <- sc_try
      G <- list(mode = "grid", scale = sc_try, lh = lhg, row = rw, fs = fz, nc = nc,
                each = each, pw = pw, br = br) }
  }
  if (is.null(G)) G <- list(mode = "grid", scale = 0)
  list(grid    = G,
       side    = list(mode = "side", scale = s_side, lw = lw, row = row_side, fs = fs_side),
       compact = list(mode = "compact", scale = s_c, lw = lw_c, row = row_c, fs = fs_c, nc = nc_c),
       stack   = list(mode = "stack", scale = s_st, lh = lh, row = row_st,
                      fs = min(7.2, row_st * 44)))
}

short_bip <- function(tbl) {
  tbl$Result <- sub(", (\\d+) EV(\\*?)(/(-?\\d+) LA)?", " \\1\\2/\\4", tbl$Result)
  tbl$Result <- sub("/$", "", tbl$Result)
  tbl
}

pa_log_table <- function(p) data.frame(
  `#` = p$PitchofPA, Pitch = p$pabbr,
  Velo = ifelse(is.na(p$RelSpeed), "--", sprintf("%.1f", p$RelSpeed)),
  Result = vapply(seq_len(nrow(p)), function(k)
    pitch_word(p$PitchCall[k], p$KorBB[k], p$PlayResult[k], p$ExitSpeed[k], p$ev_ok[k],
               p$Angle[k]), ""),
  check.names = FALSE, stringsAsFactors = FALSE)

compact_log <- function(tbl, cols, chase, row, fs, nc = 1, hollow = rep(FALSE, nrow(tbl))) {
  Ht <- vp_h(); cwid <- vp_w() / nc
  per <- ceiling(nrow(tbl) / nc)
  br <- min(0.075, row * 0.30)
  for (i in seq_len(nrow(tbl))) {
    ci <- (i - 1) %/% per; ri <- (i - 1) %% per
    xl <- ci * cwid
    yt <- Ht - row * ri; yc <- yt - row / 2
    if (ri %% 2 == 1) grid.rect(IN(xl), IN(yt), IN(cwid - 0.03), IN(row), just = c("left", "top"),
                               gp = gpar(fill = "#F6F6F6", col = NA))
    log_ball(IN(xl + 0.03 + br), IN(yc), br, cols[i], tbl[[1]][i],
             min(fs - 0.5, br * 72 * (if (tbl[[1]][i] >= 10) 1.0 else 1.25)), hollow[i])
    x0 <- IN(xl + 0.03 + 2 * br + 0.07)
    grid.text(paste(tbl$Pitch[i], tbl$Velo[i]), x0, IN(yc + row * 0.19), just = "left",
              gp = gpar(fontfamily = FONT, fontface = "bold", fontsize = fs, col = BLACK))
    grid.text(tbl$Result[i], x0, IN(yc - row * 0.21), just = "left",
              gp = gpar(fontfamily = FONT, fontface = if (chase[i]) "bold" else "plain",
                        fontsize = fs - 0.6, col = if (chase[i]) GARNET else GREY))
  }
}

grid_log <- function(tbl, cols, chase, L, hollow = rep(FALSE, nrow(tbl))) {
  Ht <- vp_h(); per <- ceiling(nrow(tbl) / L$nc)
  for (i in seq_len(nrow(tbl))) {
    ci <- (i - 1) %/% per; ri <- (i - 1) %% per
    xl <- ci * L$each; yc <- Ht - L$row * ri - L$row / 2
    if (ri %% 2 == 1) grid.rect(IN(xl), IN(yc), IN(L$each - 0.03), IN(L$row), just = c("left", "centre"),
                               gp = gpar(fill = "#F6F6F6", col = NA))
    log_ball(IN(xl + 0.02 + L$br), IN(yc), L$br, cols[i], tbl[[1]][i],
             L$br * 72 * (if (tbl[[1]][i] >= 10) 1.0 else 1.3), hollow[i])
    x0 <- xl + 0.02 + 2 * L$br + 0.06
    grid.text(paste(tbl$Pitch[i], tbl$Velo[i]), IN(x0), IN(yc), just = "left",
              gp = gpar(fontfamily = FONT, fontface = "bold", fontsize = L$fs, col = BLACK))
    grid.text(tbl$Result[i], IN(x0 + L$pw + 0.07), IN(yc), just = "left",
              gp = gpar(fontfamily = FONT, fontface = if (chase[i]) "bold" else "plain",
                        fontsize = L$fs - 0.4, col = if (chase[i]) GARNET else GREY))
  }
}

pa_head_h <- function(w) if (w < 3.3) 0.34 else 0.22

pa_panel <- function(r, S, MODE) {
  a <- r$a; p <- r$p
  set_view(p)
  W <- vp_w()
  grid.rect(gp = gpar(fill = "white", col = RULE, lwd = 0.8))
  bh <- pa_head_h(W); two <- bh > 0.3
  vs_col <- if (p$PitcherThrows[1] == "Left") GARNET else BLACK   # LHP in garnet
  inning <- sprintf("%s %s", a$half, ordinal(a$Inning))
  outs <- sprintf("%d out", p$Outs[1])
  res_col <- if (!a$complete) GARNET else if (reached(r$res)) GOOD else BLACK
  tag <- if (a$complete) sprintf("PA %d", a$pa_no) else "INC"
  pushViewport(viewport(y = 1, height = IN(bh), just = "top"))
  grid.rect(gp = gpar(fill = LIGHT, col = NA))
  if (two) {
    l1 <- paste(tag, " ", r$vs)
    fs1 <- fit_fs(l1, W - 0.14, 8.4, 5.5)
    txt(tag, IN(0.07), 0.70, fs1, GARNET, "bold", just = "left")
    txt(r$vs, IN(0.07 + tw(paste0(tag, "  "), fs1)), 0.70, fs1, vs_col, "bold", just = "left")
    l2 <- sprintf("%s  \u00b7  %s  \u00b7  %s", inning, outs, r$res)
    fs2 <- fit_fs(l2, W - 0.14, 7, 5)
    left2 <- sprintf("%s  \u00b7  %s  \u00b7  ", inning, outs)
    txt(left2, IN(0.07), 0.27, fs2, GREY, "bold", just = "left")
    txt(r$res, IN(0.07 + tw(left2, fs2)), 0.27, fs2, res_col, "bold", just = "left")
  } else {
    txt(tag, IN(0.08), 0.5, 9, GARNET, "bold", just = "left")
    txt(r$vs, IN(0.08 + tw(paste0(tag, "  "), 9)), 0.5, 9, vs_col, "bold", just = "left")
    rr <- sprintf("%s  \u00b7  %s  \u00b7  ", inning, outs)
    txt(r$res, unit(1, "npc") - IN(0.08), 0.5, 8, res_col, "bold", just = "right")
    txt(rr, unit(1, "npc") - IN(0.08 + tw(r$res, 8)), 0.5, 8, GREY, "bold", just = "right")
  }
  popViewport()

  pushViewport(viewport(y = 0, height = unit(1, "npc") - IN(bh + 0.03), just = "bottom"))
  bw <- vp_w(); bh2 <- vp_h()
  log_tbl <- pa_log_table(p)
  L <- pa_layout(bw, bh2, log_tbl)[[MODE]]
  cc <- matrix(NA_character_, nrow(p), 4)
  cc[p$chase, 4] <- GARNET

  # every zone on the page is drawn at the same scale S, centered in its space
  draw_zone <- function(zw, zh) {
    h_use <- min(zh, S * (ZHI - ZLO))
    extra <- max(0, zw / S - (ZXL + ZXR))          # spare width, split evenly
    xl <- c(-(ZXL + extra / 2), ZXR + extra / 2)
    pushViewport(viewport(y = 0.5, height = IN(h_use)))
    print(pa_zone_plot(p, r$hand, S, xl, ZLO, ZLO + h_use / S), newpage = FALSE)
    popViewport()
  }
  if (L$mode == "grid") {
    zh <- min(bh2 - L$lh - 0.02, S * (ZHI - ZLO))
    top <- (bh2 - zh - L$lh) / 2
    pushViewport(viewport(y = IN(bh2 - top), height = IN(zh), just = "top")); draw_zone(bw, zh); popViewport()
    gw <- L$nc * L$each
    pushViewport(viewport(y = IN(top), width = IN(gw), height = IN(L$lh - 0.03), just = "bottom"))
    grid_log(short_bip(log_tbl), p$pcol, p$chase, L, !p$swing)
    popViewport()
  } else if (L$mode == "compact") {
    zw <- bw - L$lw - 0.14
    pushViewport(viewport(x = 0, width = IN(zw), just = "left")); draw_zone(zw, bh2); popViewport()
    lh <- L$row * ceiling(nrow(p) / L$nc)
    pushViewport(viewport(x = IN(bw - 0.04), y = 0.5, width = IN(L$lw - 0.02), height = IN(lh),
                          just = c("right", "centre")))
    compact_log(log_tbl, p$pcol, p$chase, L$row, L$fs, L$nc, !p$swing)
    popViewport()
  } else if (L$mode == "side") {
    zw <- bw - L$lw - 0.14
    pushViewport(viewport(x = 0, width = IN(zw), just = "left")); draw_zone(zw, bh2); popViewport()
    lh <- L$row * (nrow(p) + 1)
    pushViewport(viewport(x = IN(bw - 0.06), y = 0.5, width = IN(L$lw - 0.04), height = IN(lh),
                          just = c("right", "centre")))
    table_grid(log_tbl, log_widths(log_tbl, L$fs), c("c", "c", "c", "l"), L$row, fs = L$fs,
               ball_col = p$pcol, hollow = !p$swing, col_cols = cc, pad = 0.04)
    popViewport()
  } else {
    zh <- min(bh2 - L$lh - 0.02, S * (ZHI - ZLO))
    top <- (bh2 - zh - L$lh) / 2                       # zone and log as one centered block
    pushViewport(viewport(y = IN(bh2 - top), height = IN(zh), just = "top")); draw_zone(bw, zh); popViewport()
    lwid <- min(bw - 0.10, sum(log_widths(log_tbl, L$fs)) + 0.3)
    pushViewport(viewport(y = IN(top), width = IN(lwid), height = IN(L$lh - 0.03), just = "bottom"))
    table_grid(log_tbl, log_widths(log_tbl, L$fs), c("c", "c", "c", "l"), L$row, fs = L$fs,
               ball_col = p$pcol, hollow = !p$swing, col_cols = cc, pad = 0.04)
    popViewport()
  }
  popViewport()
}

# One cell, several lines, each with its own size, color and weight
cell_lines <- function(x, yc, lines, sizes, cols, faces, just = "centre", maxw = Inf,
                       lh = 1.18) {
  sizes <- vapply(seq_along(lines), function(i)
    fit_fs(lines[i], maxw, sizes[i], 4.5, faces[i]), 0)
  hts <- sizes / 72 * lh
  y <- yc + sum(hts) / 2
  for (i in seq_along(lines)) {
    y <- y - hts[i] / 2
    grid.text(lines[i], IN(x), IN(y), just = just,
              gp = gpar(fontfamily = FONT, fontsize = sizes[i], col = cols[i], fontface = faces[i]))
    y <- y - hts[i] / 2
  }
}

# a numbered pitch ball for the pitch logs: solid on a swing, hollow on a take
log_ball <- function(x, y, r, fill, label, fs, hollow) {
  if (hollow) {
    grid.circle(x, y, IN(r * 0.9), gp = gpar(fill = "white", col = ring_col(fill), lwd = max(0.9, r * 14)))
    grid.text(label, x, y, gp = gpar(fontfamily = FONT, fontface = "bold", fontsize = fs, col = ring_col(fill)))
  } else {
    grid.circle(x, y, IN(r), gp = gpar(fill = fill, col = NA))
    grid.text(label, x, y, gp = gpar(fontfamily = FONT, fontface = "bold", fontsize = fs, col = num_col(fill)))
  }
}

`%|%` <- function(a, b) ifelse(is.na(a), b, a)
bip_log <- function(R) {
  b <- R$bip
  W <- vp_w(); Ht <- vp_h()
  # rows fill the frame: 3 or more balls use the whole box, fewer keep a 3-row height
  n_rows <- max(nrow(b), 3)
  hdr <- min(0.40, 0.24 + 0.25 * (Ht - 0.24) / n_rows * 0.5)
  row <- (Ht - hdr) / n_rows
  f1 <- function(v, fmt = "%.1f") ifelse(is.na(v), "--", sprintf(fmt, v))
  heads <- c("PA", "Result", "Pitch\nIVB / HB", "EV", "LA", "Dist", "Batted Spin", "Bat Speed\nAttack Angle")
  widths <- c(0.32, 1.26, 0.62, 0.76, 0.68, 0.46, 1.04, 0.70)
  aligns <- c("c", "l", "c", "c", "c", "c", "l", "c")
  sc <- W / sum(widths); xs <- cumsum(c(0, widths)) * sc
  xc <- function(j) if (aligns[j] == "l") xs[j] + 0.05 else xs[j] + widths[j] * sc / 2
  cw <- function(j) widths[j] * sc - 0.08
  jj <- function(j) if (aligns[j] == "l") "left" else "centre"
  grid.rect(IN(0), IN(Ht), IN(W), IN(hdr), just = c("left", "top"), gp = gpar(fill = BLACK, col = NA))
  for (j in seq_along(heads)) {
    big <- heads[j] %in% c("EV", "LA")
    hbase <- min(8.2, 5.4 + hdr * 8)
    hfs <- if (big) hbase + 3 else min(vapply(strsplit(heads[j], "\n")[[1]], fit_fs, 0,
                                        max_w = cw(j), fs = hbase, min_fs = 4.5))
    grid.text(heads[j], IN(xc(j)), IN(Ht - hdr / 2), just = jj(j),
              gp = gpar(fontfamily = FONT, fontface = "bold", col = "white",
                        fontsize = hfs, lineheight = 0.92))
  }
  # EV and LA get their own tinted columns
  for (j in 4:5) grid.rect(IN(xs[j]), IN(Ht - hdr), IN(widths[j] * sc), IN(row * nrow(b)),
                           just = c("left", "top"), gp = gpar(fill = "#FAF3F4", col = NA))
  fs  <- min(10, row * 17.5)                 # body text grows with the row
  # one size per column, so every row reads the same
  fit_all <- function(lab, j, size, face = "bold")
    min(vapply(lab, fit_fs, 0, max_w = cw(j), fs = size, min_fs = 4.5, face = face))
  log_res  <- function(x) c(`Fielder's choice` = "FC", `Reached on error` = "ROE")[x] %|% x
  res_all  <- vapply(seq_len(nrow(b)), function(i) log_res(pa_result(b[i, ])), "")
  vs_all   <- sprintf("vs. %sHP %s", substr(b$PitcherThrows, 1, 1), last_name(b$Pitcher))
  fs_vs    <- fit_all(vs_all, 2, fs - 0.4)
  sit_all  <- sprintf("%s%d  \u00b7  %d out  \u00b7  %d-%d", substr(b$half, 1, 1), b$Inning, b$Outs, b$Balls, b$Strikes)
  fs_sit   <- fit_all(sit_all, 2, fs - 0.8, "plain")
  fs_res   <- fit_all(res_all, 2, fs + 1.6)
  spin_all <- spin_type(b$HitSpinAxis, b$HitSpinRate)
  spin_ln  <- unlist(strsplit(spin_all[spin_all != "--"], "\n"))
  fs_spin  <- if (length(spin_ln)) fit_all(spin_ln, 7, fs) else fs
  big_all  <- min(26, row * 44)
  ev_all   <- ifelse(is.na(b$ExitSpeed), "--", paste0(sprintf("%.1f", b$ExitSpeed), ifelse(b$ev_ok, "", "*")))
  big_all  <- min(fit_all(c(ev_all, "99.9"), 4, big_all),
                  fit_all(c(f1(b$Angle), "-9.9"), 5, big_all))
  gap <- min(1.45, 1.18 + (row - 0.33) * 0.9)  # and the lines breathe apart
  for (i in seq_len(nrow(b))) {
    r <- b[i, ]
    yt <- Ht - hdr - row * (i - 1); yc <- yt - row / 2
    if (i %% 2 == 0) grid.rect(IN(0), IN(yt), IN(W), IN(row), just = c("left", "top"),
                               gp = gpar(fill = "#00000008", col = NA))
    grid.lines(IN(c(0, W)), IN(c(yt - row, yt - row)), gp = gpar(col = RULE, lwd = 0.5))
    # PA ball
    br <- min(0.15, row * 0.26, widths[1] * sc / 2 - 0.03)
    grid.circle(IN(xc(1)), IN(yc), IN(br), gp = gpar(fill = r$fc, col = NA))
    grid.text(r$pa_no, IN(xc(1)), IN(yc), gp = gpar(fontfamily = FONT, fontface = "bold",
              fontsize = br * 72 * 1.1, col = "white"))
    # result, situation, pitcher
    cell_lines(xc(2), yc,
      c(res_all[i], sit_all[i], vs_all[i]),
      c(fs_res, fs_sit, fs_vs),
      c(if (r$PlayResult %in% HITS) GOOD else BLACK, GREY,
        if (r$PitcherThrows == "Left") GARNET else BLACK),
      c("bold", "plain", if (r$PitcherThrows == "Left") "bold" else "plain"), "left", cw(2), gap)
    # pitch
    cell_lines(xc(3), yc,
      c(sprintf("%s %s", r$pabbr, f1(r$RelSpeed)),
        sprintf("%s / %s", f1(r$InducedVertBreak), f1(r$HorzBreak))),
      c(fs + 0.6, fs - 0.8), c(BLACK, GREY), c("bold", "plain"), maxw = cw(3), lh = gap)
    # EV and LA, the headline numbers
    hard <- r$ev_ok && !is.na(r$ExitSpeed) && r$ExitSpeed >= hard_hit_ev
    sweet <- !is.na(r$Angle) && r$Angle >= sweet_lo && r$Angle <= sweet_hi
    big <- big_all
    ev_s <- ifelse(is.na(r$ExitSpeed), "--",
                   paste0(sprintf("%.1f", r$ExitSpeed), if (r$ev_ok) "" else "*"))
    la_s <- f1(r$Angle)
    unit_y <- if (row >= 0.40) yc - big / 72 * 0.62 else NA
    num_y  <- if (is.na(unit_y)) yc else yc + 0.05
    for (u in list(c(4, "mph"), c(5, "deg"), c(6, "ft"))) if (!is.na(unit_y))
      grid.text(u[2], IN(xc(as.integer(u[1]))), IN(unit_y - 0.02),
                gp = gpar(fontfamily = FONT, fontsize = min(7, fs - 1.5), col = GREY))
    grid.text(ev_s, IN(xc(4)), IN(num_y), gp = gpar(fontfamily = FONT, fontface = "bold",
              fontsize = big, col = if (is.na(r$ExitSpeed)) GREY else ev_tier(r$ExitSpeed)))
    grid.text(la_s, IN(xc(5)), IN(num_y), gp = gpar(fontfamily = FONT, fontface = "bold",
              fontsize = big, col = la_tier(r$Angle)))
    grid.text(f1(r$Distance, "%.0f"), IN(xc(6)), IN(num_y),
              gp = gpar(fontfamily = FONT, fontface = "bold",
                        fontsize = fit_fs("388", cw(6), fs + 2.5, 5), col = BLACK))
    # batted spin: the words and the numbers together
    st <- spin_type(r$HitSpinAxis, r$HitSpinRate)
    nums <- if (is.na(r$HitSpinRate)) "no spin read" else if (is.na(r$HitSpinAxis))
      sprintf("%.0f rpm  \u00b7  no axis", r$HitSpinRate) else
      sprintf("%.0f rpm  \u00b7  %.0f\u00b0", r$HitSpinRate, r$HitSpinAxis %% 360)
    wl <- if (st == "--") character() else strsplit(st, "\n")[[1]]
    cell_lines(xc(7), yc, c(wl, nums),
               c(rep(fs_spin, length(wl)), min(fs - 0.6, fs_spin)),
               c(rep(BLACK, length(wl)), GREY),
               c(rep("bold", min(1, length(wl))), rep("plain", max(0, length(wl) - 1)), "plain"), "left", cw(7), gap)
    # bat
    cell_lines(xc(8), yc,
      c(ifelse(is.na(r$BatSpeed), "--", sprintf("%.1f", r$BatSpeed)),
        ifelse(is.na(r$VerticalAttackAngle), "--", sprintf("%.1f\u00b0", r$VerticalAttackAngle))),
      c(fs + 0.6, fs - 0.8), c(BLACK, GREY), c("bold", "plain"), maxw = cw(8), lh = gap)
  }
  # column rules
  for (j in 2:length(widths))
    grid.lines(IN(rep(xs[j], 2)), IN(c(Ht - hdr, Ht - hdr - row * nrow(b))),
               gp = gpar(col = RULE, lwd = 0.5))
}

FOOT_FS <- 4.6
draw_footer <- function() {
  txt <- function(label, x, y, size, col, just, ...) {
    avail <- vp_w() - if (y < 0.3) 2.9 else 0
    grid.text(label, x, y, just = just, gp = gpar(fontfamily = FONT, col = col,
              fontsize = fit_fs(label, avail, size, 3.5, "plain")))
  }
  txt(sprintf(paste0("2027 NCAA ABS static zone:  19\" wide, %.2f\" to %.2f\" high, 3.02\" ball, lines drawn at the ball edge so any part ",
      "of the ball touching the box is in the zone.  Heart: the shaded middle %d\" (%d\" either side of center), the full height of the zone; any part of the ball touching it counts, same rule as the zone."),
      REF_BOT, REF_TOP, 2 * MID_HALF, MID_HALF),
      0, 0.82, FOOT_FS, GREY, just = "left")
  txt(paste0("Chase = swing out of the zone (navy in the pitch logs).  Uncompetitive = more than ", UNCOMP, "\" outside the zone, drawn at the edge of the view.  Hard hit = ", hard_hit_ev, "+ mph.  ",
      "* low confidence or estimated exit velo, left out of averages.  ",
      "Spin type reads the batted ball spin axis on the pitch scale: 180\u00b0 backspin, 0\u00b0 topspin, ",
      "90\u00b0 RH SL spin (hooks toward the LF line), 270\u00b0 LH SL spin (toward the RF line)."),
      0, 0.50, FOOT_FS, GREY, just = "left")
  txt(paste0("Contact point: overhead, feet from the plate tip.  Spray chart: ", STADIUM_NAME,
             ", balls numbered by PA.  EV: 95+ green, down to red under 80.  LA: 10-30\u00b0 green, one tier down per 5\u00b0 high or low."),
      0, 0.18, FOOT_FS, GREY, just = "left")
  txt("Umpire's view, to scale   |   Data: TrackMan   |   Georgia Southern Baseball Analytics", 1, 0.18, FOOT_FS, GREY, just = "right")
}

# Batted ball mix: trajectory (GB / LD / FB / PU) and direction (pull / middle /
# oppo) for every ball in play. Bunts count as ground balls. Direction is
# relative to the batter's side for that ball; middle is within MIX_MID degrees.
MIX_MID <- 15

# Swing decisions: how often he swung in the heart, on the rest of the zone (edge),
# and out of the zone (chase). The process target: attack the heart, take chase.
swing_decisions <- function(T) {
  pushViewport(panel("SWING DECISIONS", NULL))
  H <- vp_h(); W <- vp_w()
  # heart: we want more swings; edge and chase: we want fewer
  rows <- list(
    list(l = HEART_LABEL, g = "swing more", sw = T$mid_sw, n = T$mid_n, col = GOOD),
    list(l = "Edge", g = "swing less", sw = T$edge_sw, n = T$edge_n, col = "#E07B28"),
    list(l = "Chase", g = "swing less", sw = T$chase, n = T$ooz, col = GARNET))
  rh <- (H - 0.06) / 3
  for (i in seq_along(rows)) {
    r <- rows[[i]]
    yt <- H - 0.03 - rh * (i - 1)
    pct <- if (r$n) r$sw / r$n else NA
    lf <- min(8, rh * 26)
    txt(r$l, IN(0.06), IN(yt - rh * 0.26), lf, BLACK, "bold", just = "left")
    txt(r$g, IN(0.06 + tw(r$l, lf) + 0.05), IN(yt - rh * 0.26), min(6.2, lf - 1.4),
        r$col, "bold", just = "left")
    lab <- if (r$n) sprintf("%d/%d", r$sw, r$n) else "--"
    txt(lab, IN(W - 0.06), IN(yt - rh * 0.26), min(8.5, rh * 27), r$col, "bold", just = "right")
    bw <- W - 0.12; by <- yt - rh * 0.66; bh <- min(0.09, rh * 0.26)
    grid.rect(IN(0.06), IN(by), IN(bw), IN(bh), just = c("left", "centre"),
              gp = gpar(fill = "#EFEFEF", col = NA))
    if (!is.na(pct) && pct > 0)
      grid.rect(IN(0.06), IN(by), IN(bw * pct), IN(bh), just = c("left", "centre"),
                gp = gpar(fill = r$col, col = NA))
    if (!is.na(pct)) {
      inside <- pct > 0.7
      txt(sprintf("%.0f%%", 100 * pct),
          IN(if (inside) 0.06 + bw * pct - 0.03 else 0.06 + max(bw * pct, 0) + 0.03), IN(by),
          min(5.8, rh * 18), if (inside) "white" else GREY, "bold",
          just = if (inside) "right" else "left")
    }
  }
  popViewport()
}
batted_mix <- function(b) {
  pushViewport(panel("BATTED BALL MIX", NULL))
  H <- vp_h(); W <- vp_w()
  if (!nrow(b)) { txt("No balls in play", 0.5, 0.5, 7, GREY, "italic"); popViewport(); return(invisible()) }
  ht <- ifelse(b$TaggedHitType %in% c("Undefined", NA), b$AutoHitType, b$TaggedHitType)
  traj <- c(GB = sum(ht %in% c("GroundBall", "Bunt")), LD = sum(ht == "LineDrive"),
            FB = sum(ht == "FlyBall"), PU = sum(ht == "Popup"))
  pull_dir <- ifelse(b$BatterSide == "Left", b$Direction, -b$Direction)
  dirs <- c(PULL = sum(pull_dir > MIX_MID, na.rm = TRUE),
            MID  = sum(abs(pull_dir) <= MIX_MID, na.rm = TRUE),
            OPPO = sum(pull_dir < -MIX_MID, na.rm = TRUE))
  chips <- function(v, ytop, hgt) {
    n <- length(v); cw <- (W - 0.06) / n
    for (i in seq_len(n)) {
      x0 <- 0.03 + (i - 1) * cw
      on <- v[i] > 0
      grid.rect(IN(x0 + 0.01), IN(ytop), IN(cw - 0.02), IN(hgt), just = c("left", "top"),
                gp = gpar(fill = if (on) "#FAF3F4" else "#F6F6F6", col = NA))
      grid.text(v[i], IN(x0 + cw / 2), IN(ytop - hgt * 0.40),
                gp = gpar(fontfamily = FONT, fontface = "bold", col = if (on) GARNET else "#B5B5B5",
                          fontsize = min(15, hgt * 50)))
      grid.text(names(v)[i], IN(x0 + cw / 2), IN(ytop - hgt * 0.82),
                gp = gpar(fontfamily = FONT, fontface = "bold", col = GREY,
                          fontsize = fit_fs(names(v)[i], cw - 0.04, min(6, hgt * 20), 3.8)))
    }
  }
  hg <- (H - 0.10) / 2
  chips(traj, H - 0.03, hg)
  chips(dirs, H - 0.07 - hg, hg)
  popViewport()
}

BOT_H     <- 2.42   # bottom slice: spray chart | contact point | batted ball log
SPRAY_W   <- 2.45
CONTACT_W <- 1.05
draw_page <- function(R) {

  M <- 0.30
  grid.newpage()
  pushViewport(viewport(width = IN(8.5 - 2 * M), height = IN(11 - 2 * M)))
  H <- 11 - 2 * M; W <- 8.5 - 2 * M; g <- 0.08

  hdr_h  <- HDR_H
  line_h <- 0.76
  leg_h  <- 0.16
  inc_h  <- if (length(R$inc_notes) || R$over) 0.13 * (length(R$inc_notes) + (R$over > 0)) + 0.04 else 0
  bot_h  <- BOT_H
  foot_h <- 0.30
  pa_h   <- H - hdr_h - line_h - leg_h - inc_h - bot_h - foot_h - 5 * g

  y <- H
  pushViewport(viewport(y = IN(y), height = IN(hdr_h), just = "top")); draw_header(R$bn); popViewport()
  y <- y - hdr_h - g - 0.02
  pushViewport(viewport(y = IN(y), height = IN(line_h), just = "top")); game_line(R); popViewport()
  y <- y - line_h - g
  pushViewport(viewport(y = IN(y), height = IN(leg_h), just = "top")); legend_strip(R$pcs); popViewport()
  y <- y - leg_h - g * 0.6

  # ---- plate appearances ----
  rows <- slot_rows(length(R$pa_rows))
  gap <- 0.07
  rh <- (pa_h - gap * (length(rows) - 1)) / length(rows)
  # one layout and one zone scale for the whole page: the mode whose smallest
  # zone is largest wins, and every panel draws at that scale
  modes <- c(side = Inf, compact = Inf, stack = Inf, grid = Inf); k <- 0
  for (ri in seq_along(rows)) {
    cw <- (W - gap * (rows[ri] - 1)) / rows[ri]
    for (ci in seq_len(rows[ri])) {
      k <- k + 1
      set_view(R$pa_rows[[k]]$p)
      L <- pa_layout(cw, rh - pa_head_h(cw) - 0.03, pa_log_table(R$pa_rows[[k]]$p))
      for (m in names(modes)) modes[m] <- min(modes[m], L[[m]]$scale)
    }
  }
  # tie breaks: the full table is preferred, the one-line grid only when it clearly helps
  bias <- c(side = 1.04, compact = 1, stack = 1, grid = 0.93)
  MODE <- names(which.max(modes * bias[names(modes)])); S <- modes[[MODE]]
  k <- 0
  for (ri in seq_along(rows)) {
    nc <- rows[ri]; cw <- (W - gap * (nc - 1)) / nc
    for (ci in seq_len(nc)) {
      k <- k + 1
      pushViewport(viewport(x = IN((ci - 1) * (cw + gap)), y = IN(y - (ri - 1) * (rh + gap)),
                            width = IN(cw), height = IN(rh), just = c("left", "top")))
      pa_panel(R$pa_rows[[k]], S, MODE)
      popViewport()
    }
  }
  y <- y - pa_h - g * 0.6
  if (inc_h > 0) {
    notes <- c(R$inc_notes, if (R$over) sprintf(
      "%d more appearance%s in this game not shown; the page holds %d. All pitches still count in the game line.",
      R$over, ifelse(R$over == 1, "", "s"), MAX_SLOTS))
    for (i in seq_along(notes))
      txt(notes[i], IN(0.02), IN(y - 0.02 - 0.13 * (i - 0.5)),
          fit_fs(notes[i], W - 0.05, 7, 5, "italic"), GARNET, "bold.italic", just = "left")
    y <- y - inc_h
  }
  y <- y - g * 0.4

  # ---- bottom slice: spray chart | contact point | batted ball log ----
  sf <- spray_frame()
  bar <- 0.22
  spw <- min(SPRAY_W, (bot_h - bar - 0.06) * diff(sf$xlim) / diff(sf$ylim) + 0.08)
  n_sp <- sum(is.na(R$bip$Distance) | is.na(R$bip$Direction))
  pushViewport(viewport(x = 0, y = IN(y), width = IN(spw), height = IN(bot_h), just = c("left", "top")))
  pushViewport(panel("SPRAY CHART", if (n_sp) sprintf("%d not tracked", n_sp) else STADIUM_NAME))
  pw <- vp_w(); ph <- vp_h()
  sc <- min(pw / diff(sf$xlim), ph / diff(sf$ylim))
  print(spray_plot(R$bip, 0.085 / sc), newpage = FALSE)
  if (!nrow(R$bip)) txt("No balls in play", 0.5, 0.35, 8, "white", "bold.italic")
  popViewport(2)

  n_ct <- sum(is.na(R$bip$ContactPositionX))
  pushViewport(viewport(x = IN(spw + 0.08), y = IN(y), width = IN(CONTACT_W), height = IN(bot_h),
                        just = c("left", "top")))
  # contact point on top at its natural size; batted ball mix below
  ct_ar <- (CT_FRAME$yhi - CT_FRAME$ylo) / (2 * CT_FRAME$hw)
  ct_h <- min(bot_h * 0.62, (CONTACT_W - 0.04) * ct_ar + 0.26)
  pushViewport(viewport(y = 1, height = IN(ct_h), just = "top"))
  pushViewport(panel("CONTACT POINT", if (n_ct) sprintf("%d missing", n_ct) else NULL))
  if (nrow(R$bip)) {
    pw <- vp_w(); ph <- vp_h()
    sc <- min(pw / (2 * CT_FRAME$hw), ph / (CT_FRAME$yhi - CT_FRAME$ylo))
    print(contact_plot(R$bip, R$hands, 0.070 / sc), newpage = FALSE)
  } else txt("No balls in play", 0.5, 0.5, 7, GREY, "italic")
  popViewport(2)
  pushViewport(viewport(y = 0, height = IN(bot_h - ct_h - 0.07), just = "bottom"))
  swing_decisions(R$T)
  popViewport(2)

  lx <- spw + CONTACT_W + 0.16
  pushViewport(viewport(x = IN(lx), y = IN(y), width = IN(W - lx), height = IN(bot_h),
                        just = c("left", "top")))
  sub <- sprintf("%d ball%s in play%s", nrow(R$bip) + R$bip_over,
                 ifelse(nrow(R$bip) + R$bip_over == 1, "", "s"),
                 if (R$bip_over) sprintf(", first %d shown", MAX_BIP_ROWS) else "")
  pushViewport(panel("BATTED BALL LOG", sub, GARNET))
  if (nrow(R$bip)) {
    pushViewport(viewport(width = unit(1, "npc") - IN(0.06), height = unit(1, "npc") - IN(0.03)))
    bip_log(R)
    popViewport()
  } else txt("No balls in play", 0.5, 0.5, 8.5, GREY, "italic")
  popViewport(2)

  pushViewport(viewport(y = 0, height = IN(foot_h), just = "bottom")); draw_footer(); popViewport()
  popViewport()
}

# ---- 12. RUN -------------------------------------------------------------------
roster <- if (nzchar(batter_name)) batter_name else
  raw %>% filter(BatterTeam == my_team) %>% arrange(PitchNo) %>% pull(Batter) %>% unique()
if (!length(roster)) stop("No batter found for ", my_team, " in ", csv_path)
missing <- setdiff(roster, raw$Batter)
if (length(missing)) stop("Batter not in this file: ", paste(missing, collapse = ", "))

reports <- lapply(roster, build_batter)
if (combined_pdf && length(reports) > 1) {
  comb <- file.path(out_dir, sprintf("%s_Hitter_Reports_%s.pdf",
                    gsub("\\s+", "_", tname(my_team)), format(game_date, "%Y%m%d")))
  pdf(comb, width = 8.5, height = 11, family = FONT, useDingbats = FALSE)
  for (R in reports) draw_page(R)
  invisible(dev.off())
}
for (R in reports) {
  f <- file.path(out_dir, sprintf("%s_Hitter_Report_%s.pdf",
                 gsub("\\s+", "_", disp_name(R$bn)), format(game_date, "%Y%m%d")))
  pdf(f, width = 8.5, height = 11, family = FONT, useDingbats = FALSE)
  draw_page(R)
  invisible(dev.off())
  inc <- sum(!vapply(R$pa_rows, function(r) r$a$complete, TRUE))
  cat(sprintf("%-24s %d PA, %d free%s | %d pitches, %d chase, %d whiff | %d BIP  -> %s\n",
              disp_name(R$bn), R$PA, R$BB + R$HBP,
              if (inc) sprintf(" (+%d incomplete)", inc) else "",
              R$T$pitches, R$T$chase, R$T$whiffs, R$T$bip, f))
}
if (combined_pdf && length(reports) > 1) cat(sprintf("All batters -> %s\n", comb))
