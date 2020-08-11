merge_target_ref <-
function(target, ref,
                             name_female = "Female",
                             name_male = "Male") {
    target[, 1] <- factor(target[, 1])
    levels(target[, 1]) <- paste("Target (", rownames(target), ")", sep = "")
    ref <- droplevels(ref)
    levels(ref[, 1]) <- c(name_female, name_male)
    return(rbind(target[, colnames(ref)], ref))
}
