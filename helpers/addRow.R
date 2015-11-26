
addRow = function (t, r) {
    if (is.null (t) == TRUE) {
        t = r 
    } else {
        t = rbind (t, r)
    }
    return (t)
}