FUNCTION cropme, input, pix

    s = size(input,/d)
    nrow = s[0]
    ncol = s[1]
    output = input[(nrow-1)/2 - pix:(nrow-1)/2 + pix,(ncol-1)/2 - pix:(ncol-1)/2 + pix]

return, output
end