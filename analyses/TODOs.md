# TO DOs

## Issues

### ValidateDup needs to merge these columns:

- scientificNameAuthorship
- id

### Gazetteer has duplicated loc.correct

[23] "brazil_sao paulo_porto ferreira_pe porto ferreira (antiga reserva estadual porto ferreira)"
[24] "brazil_sao paulo_porto ferreira_parque estadual porto ferreira"

### checkInverted error if col classes are wrong

Error in -tmp[, lon] : argumento inválido para operador unário

fix: use as.numeric

### checkDist error if there are duplicated name/author pair in flora

This could be solved adding multiple="any", "first" ou "last"

        x1 <- dplyr::left_join(x1, plantR::bfoNames[, key.cols],
            by = stats::setNames(c("tax.name", "tax.authorship"),
                c(tax.name, tax.author)), keep = TRUE)

Sugestion: if the join returned bigger size, show a warning and redo the join with multiple="first"

## getOccs

- add grepl for NAME_3 -> done

## locationTables

- remove all locations that match grep(uc_string) -> done?? maybe redo later
- search database for all occurrences of "estação experimental", "floresta d", 'eec", etc