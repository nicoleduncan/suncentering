pro ff

    xpos = 210
    ypos = 153

    a=READ_TIFF('plots_tables_images/dimsun1.tiff',channels=1)


    wholeimage = BYTSCL(a)
    rad = 22

    crop = wholeimage[xpos-rad:xpos+rad,ypos-rad:ypos+rad]

    s = SIZE(crop,/dim)
    nrow = s[0]
    ncol = s[1]

    xpb = (SHIFT_DIFF(EMBOSS(crop),dir=3)) lt -80
    ypb = (SHIFT_DIFF(EMBOSS(crop, az=90),dir=1)) lt -80

    ; The sunthetic image has too-nice edges that they end up being edge-detected 
    ; So I actually didn't anticipate this.

    window,0
    !p.multi=[0,2,1]
    cgimage,xpb*crop,/k
    cgimage,ypb*crop,/k
    !p.multi=0


    ind_col = WHERE(xpb eq 1) mod ncol
    ind_row = WHERE(ypb eq 1)/nrow


    a = mode(ind_col)
    b = ind_col[WHERE(ind_col ne a)]
    c = mode(b)

    f = mode(ind_row)
    g = ind_row[WHERE(ind_row ne f)]
    h = mode(g)


    ; Just to make it sorted
    xpos = [a,c]
    ypos = [f,h]
    xpos = xpos[SORT(xpos)]
    ypos = ypos[SORT(ypos)]

    ; Because fiducials are 2 pixels wide 
    xmask = [xpos[0]-1,xpos[0],xpos[1]-1,xpos[1]]
    ymask = [ypos[0]-1,ypos[0],ypos[1]-1,ypos[1]]

    ftest = crop
    ; ftest[a,*] = 100
    ; ftest[c,*] = 100

    ps_start,filename='mask_outline.eps',/color,/encapsulated,xsize=6,ysize=6,/inches
        display,ftest,/square
        plot_edges,xpb,thick=6,setcolor=80
        plot_edges,ypb,thick=6,setcolor=255
    ps_end,resize=100

    stop
END


pro dim3
    ;Reads parameters from csv file and converts strings into varilables. BRILLIANT.
    readcol,'param.txt',var,num,format='A,F',delimiter=' '
    for i=0,N_ELEMENTS(var)-1 do (SCOPE_VARFETCH(var[i],/enter,level=0))=num[i]
    
    ; Here we make the assumption that the darker regions are linearly darker so we can just divide by 2 and 4
    ; Works pretty well
    wholeimage = BYTSCL( READ_TIFF('plots_tables_images/dimsun1.tiff',channels=1) )
    ideal = BYTSCL( READ_TIFF('plots_tables_images/dimsun_ideal.tiff',channels=1) )
    crop = wholeimage[xpos-rad:xpos+rad,ypos-rad:ypos+rad]
    icrop = ideal[xpos-rad:xpos+rad,ypos-rad:ypos+rad]
    imask = icrop lt idealthresh
    ; dim50 = .5*crop
    dim50 = wholeimage[xpos50-rad:xpos50+rad,ypos50-rad:ypos50+rad]
    ; dim25 = .25*crop
    dim25 = wholeimage[xpos25-rad:xpos25+rad,ypos25-rad:ypos25+rad]
    ; Actually using the wholeimage cropped areas reveal little difference than with the cheating method

    s = SIZE(crop,/dim)
    nrow = s[0]
    ncol = s[1]

    xpb = (SHIFT_DIFF(EMBOSS(crop),dir=3)) lt thresh
    ypb = (SHIFT_DIFF(EMBOSS(crop, az=90),dir=1)) lt thresh

    ; -80 is about 3 stddev() above the minimum
    ; -80 is also about half the minimum of xpb/ypb

    xpb = (SHIFT_DIFF(EMBOSS(dim50),dir=3)) lt thresh/2
    ypb = (SHIFT_DIFF(EMBOSS(dim50, az=90),dir=1)) lt thresh/2
    
    ; ps_start,filename='plots_tables_images/dim50.eps',/color,/encapsulated,xsize=8,ysize=8,/inches
    display,byte(dim50),/square,title='50% Dim'
    plot_edges,xpb,thick=6,setcolor=80
    plot_edges,ypb,thick=6,setcolor=255
    ; ps_end,resize=100

    xpb = (SHIFT_DIFF(EMBOSS(dim25),dir=3)) lt thresh/4
    ypb = (SHIFT_DIFF(EMBOSS(dim25, az=90),dir=1)) lt thresh/4
    ; ps_start,filename='plots_tables_images/dim25.eps',/color,/encapsulated,xsize=8,ysize=8,/inches       
    display,byte(dim25),/square,title='25% Dim'
    plot_edges,xpb,thick=6,setcolor=80
    plot_edges,ypb,thick=6,setcolor=255
    ; ps_end,resize=100
    
    
    ; Working with image of blank sun with real fiducials:
    whitecrop = bytarr(s) + 198  ;198 is the mode of the not-fiducial-maskt
    fakesun = imask*crop + whitecrop*(icrop gt idealthresh)
    ; cgsurface,(SHIFT_DIFF(EMBOSS(fakesun),dir=3))
    a = SHIFT_DIFF(EMBOSS(fakesun),dir=3)
    cgimage,a,/k
    cgimage,a*(a gt 10),/k
    stop
    ; The sunthetic image has too-nice edges that they end up being edge-detected 
    ; So I actually didn't anticipate this.

    ; window,0
    ; !p.multi=[0,2,1]
    ; cgimage,xpb*crop,/k
    ; cgimage,ypb*crop,/k
    ; !p.multi=0

    ind_col = WHERE(xpb eq 1) mod ncol
    ind_row = WHERE(ypb eq 1)/nrow


    a = mode(ind_col)
    b = ind_col[WHERE(ind_col ne a)]
    c = mode(b)

    f = mode(ind_row)
    g = ind_row[WHERE(ind_row ne f)]
    h = mode(g)


    ; Just to make it sorted
    xpos = [a,c]
    ypos = [f,h]
    xpos = xpos[SORT(xpos)]
    ypos = ypos[SORT(ypos)]

    ; Because fiducials are 2 pixels wide 
    xmask = [xpos[0]-1,xpos[0],xpos[1]-1,xpos[1]]
    ymask = [ypos[0]-1,ypos[0],ypos[1]-1,ypos[1]]

    ; ps_start,filename='mask_outline.eps',/color,/encapsulated,xsize=6,ysize=6,/inches
    ;     display,crop,/square
    ;     plot_edges,xpb,thick=6,setcolor=80
    ;     plot_edges,ypb,thick=6,setcolor=255
    ; ps_end,resize=100
end    