function centroidtest, input

a=setbetterthresh(input)
b=quickmask(input,a.thresh100)
input[b.xpos - !param.crop_box:b.xpos + !param.crop_box,$
    b.ypos - !param.crop_box:b.ypos + !param.crop_box] =0
c=quickmask(input,a.thresh50)
input[c.xpos - !param.crop_box:c.xpos + !param.crop_box,$
    c.ypos - !param.crop_box:c.ypos + !param.crop_box] =0
d=quickmask(input,a.thresh25)

return,{reg1:b,reg2:c,reg3:d}
end