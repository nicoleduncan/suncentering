function howfar, from, to
; program that computes the distance between two points

xd = from[0] - to[0]
yd = from[1] - to[1]

return,sqrt(xd^2 + yd^2)
end