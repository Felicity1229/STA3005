#' Mushroom Classification Dataset
#'
#' This dataset includes descriptions of hypothetical samples corresponding to 23 species
#' of gilled mushrooms in the Agaricus and Lepiota Family Mushroom drawn from The Audubon
#' Society Field Guide to North American Mushrooms (1981). The data is commonly used to
#' predict whether a mushroom is edible or poisonous based on its physical characteristics.
#' Note that there is no simple rule for determining the edibility of a mushroom.
#'
#' @format A data frame with 23 columns (1 target variable and 22 predictors):
#' \describe{
#'   \item{class}{Target variable indicating edibility: e = edible, p = poisonous (includes unknown edibility)}
#'   \item{cap.shape}{Shape of the cap: b = bell, c = conical, x = convex, f = flat, k = knobbed, s = sunken}
#'   \item{cap.surface}{Surface texture of the cap: f = fibrous, g = grooves, y = scaly, s = smooth}
#'   \item{cap.color}{Color of the cap: n = brown, b = buff, c = cinnamon, g = gray, r = green, p = pink, u = purple, e = red, w = white, y = yellow}
#'   \item{bruises}{Indicates whether the mushroom has bruises: t = bruises, f = no}
#'   \item{odor}{Smell of the mushroom: a = almond, l = anise, c = creosote, y = fishy, f = foul, m = musty, n = none, p = pungent, s = spicy}
#'   \item{gill.attachment}{Attachment of the gills: a = attached, d = descending, f = free, n = notched}
#'   \item{gill.spacing}{Spacing of the gills: c = close, w = crowded, d = distant}
#'   \item{gill.size}{Size of the gills: b = broad, n = narrow}
#'   \item{gill.color}{Color of the gills: k = black, n = brown, b = buff, h = chocolate, g = gray, r = green, o = orange, p = pink, u = purple, e = red, w = white, y = yellow}
#'   \item{stalk.shape}{Shape of the stalk: e = enlarging, t = tapering}
#'   \item{stalk.root}{Root of the stalk: b = bulbous, c = club, u = cup, e = equal, z = rhizomorphs, r = rooted, ? = missing}
#'   \item{stalk.surface.above.ring}{Stalk surface texture above the ring: f = fibrous, y = scaly, k = silky, s = smooth}
#'   \item{stalk.surface.below.ring}{Stalk surface texture below the ring: f = fibrous, y = scaly, k = silky, s = smooth}
#'   \item{stalk.color.above.ring}{Stalk color above the ring: n = brown, b = buff, c = cinnamon, g = gray, o = orange, p = pink, e = red, w = white, y = yellow}
#'   \item{stalk.color.below.ring}{Stalk color below the ring: n = brown, b = buff, c = cinnamon, g = gray, o = orange, p = pink, e = red, w = white, y = yellow}
#'   \item{veil.type}{Type of veil: p = partial, u = universal}
#'   \item{veil.color}{Color of the veil: n = brown, o = orange, w = white, y = yellow}
#'   \item{ring.number}{Number of rings: n = none, o = one, t = two}
#'   \item{ring.type}{Type of ring: c = cobwebby, e = evanescent, f = flaring, l = large, n = none, p = pendant, s = sheathing, z = zone}
#'   \item{spore.print.color}{Color of the spore print: k = black, n = brown, b = buff, h = chocolate, r = green, o = orange, u = purple, w = white, y = yellow}
#'   \item{population}{Population spread: a = abundant, c = clustered, n = numerous, s = scattered, v = several, y = solitary}
#'   \item{habitat}{Environment where found: g = grasses, l = leaves, m = meadows, p = paths, u = urban, w = waste, d = woods}
#' }
#' @source \url{https://www.kaggle.com/datasets/uciml/mushroom-classification}
"mushroom_dataset"
