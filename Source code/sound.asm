    name "kernel"
    ; directive to create bin file
    #make_bin#
    
    ;where our kernel is located, to help our bootloader to load it properly
    #load_segment=0800#
    #load_offset=0000#
    
    ;We define default values for the registers
    #al=0b#
    #ah=00#
    #bh=00#
    #bl=00#
    #ch=00#
    #cl=02#
    #dh=00#
    #dl=00#
    #ds=0800#
    #es=0800#
    #si=7c02#
    #di=0000#
    #bp=0000#
    #cs=0800#
    #ip=0000#
    #ss=07c0#
    #sp=03fe#
    
        
    org 0000h  ;As specified above, the offset of our program is 0000h
     
    ;We make sure that ds is equal to cs   
    push    cs
    pop     ds
    
    ;Set the video mode for text output
    mov     ah, 00h
    mov     al, 03h
    int     10h
    
    ;Blinking disabled for compatibility with dos/bios,
    ;emulator and windows prompt never blink
    mov     ax, 1003h
    mov     bx, 0      ; disable blinking
    int     10h
    
    nop  
    
    
    ;Print the welcoming message showing several options for the user to choose
    printTheMessage:
    lea si, msg  ;load the address of the welcome message
    CALL printString   
        
    mov ah, 00h
    int 16h      ;Interrupt to wait for a keyboard input
    
    CMP ah, 01h  ;Escape
    je rebootTheSystem
    
    CMP ah, 02h ;Key 1
    je goOn
    
    CMP ah, 03h ;Key 2
    je watchMePlaySubMenu
 
    
    CMP ah, 04h  ;Key 3
    je trainingSubMenu
    
    CMP ah, 05h  ;Key 4
    je help
    
    CALL clearWindow
    lea si, tryAgainMsg ;load the address of the try again message
    CALL printString
    jmp printTheMessage ;jump back to the beginning as the input was unsupported   
    
    rebootTheSystem:
    jmp	0ffffh:0000h  ;reboot the system, redirecting to our bootloader
    
    ;Pre-load free to play mode	
        
    goOn:
    
    CALL getThePianoReady
    
    jmp freeToPlay 
    
   
    ;Pre-load watch me play mode by asking which music to load
    
    watchMePlaySubMenu:
    CALL clearWindow
    
    lea si, watchMePlaySubMenuMsg ;load the address of the watch me play submenu message
    CALL printString   
    
    ;We wait for the user to make its choice    
    mov ah, 00h
    int 16h  
    
    CMP ah, 01h
    je rebootTheSystem  ;Escape has been pressed, so we reboot
    
    CMP ah, 04h
    jle loadTheTrack     ;The user has pressed one of the right key, so we carry on with the right track
    
    CALL clearWindow
    jmp watchMePlaySubMenu  ;wrong choice, we get back to ask the user for a right anwser again     
    
    loadTheTrack: 
    CALL loadTheMusic 
    CALL getThePianoReady 
    jmp watchMePlay      ;Everything is ready for the watch me play mode, so we move along
    
    
    ;Pre-load training mode by asking which music to load 
                            
    trainingSubMenu: 
    CALL clearWindow
    
    lea si, trainingSubMenuMsg ;load the address of the training submenu message
    CALL printString   
    
    ;We wait for the user to make its choice 
    mov ah, 00h
    int 16h 
          
    CMP ah, 01h
    je rebootTheSystem  ;Escape, so we reboot
    
    CMP ah, 04h
    jle loadTheTrackTraining   ;Right key pressed, so we load the requested track
    
    CALL clearWindow
    jmp trainingSubMenu      
    
    loadTheTrackTraining:    
    CALL loadTheMusic
    CALL getThePianoReady 
    jmp trainingMode     ;The track has been loaded, so we can start the training mode  
    
    
    help: 
    CALL clearWindow
    
    lea si, helpMsg  ;load the address of the help message
    CALL printString 
    
    waitForTheKeyPress:
    
    ;We wait for a key to be pressed, mostly escape if the user want to reboot
    mov ah, 00h
    int 16h 
    
    CMP ah,01h
    je rebootTheSystem ;Escape has been pressed, so we reboot to quit the help
    
    jmp waitForTheKeyPress ;The user has pressed another key, so we get back waiting for another
    
    
    
    ;Free to play mode where the user can play whatever he wants
    
    freeToPlay:
    
    ;We wait for a key to be pressed
    mov ah, 00h
    int 16h
    
    CMP ah, 01h
    je rebootTheSystem ;Escape has been pressed, so we reboot to quit the free to play mode   
    
    ;Another key has been pressed, so we try to define the corresponding frequency
    
    CALL defineSound 
    
    ;The sound is played and the key is redrawn on the screen if the key is mapped, otherwise nothing happens
    
    CALL playSound
      
    jmp freeToPlay  ;We jump back to the start waiting for the user to press another key

    
    ;Watch me play mode where the computer plays a track chosen by the user
     
    watchMePlay:
    CMP [si], 0  ;We compare the current note (key code actually) of the track to zero which is marking the end of the track
    je rebootTheSystem  ;We hit the zero, thus the track is over, so we reboot the system to quit the watch me play mode 
    
    mov ah, [si]  ;We load the value of the current key code in ah at the address store in si, the one of the current key code in our array
    push si     ;We put the value of si in the stack as it's going to be changed in the procedures below
    
    ;We play the sound and redraw the key, as the key code has been store in ah
    
    CALL defineSound 
    
    CALL playSound
    
    ;We put back the right value of si stored in the stack in it and we increment it to get the next note of the track
    pop si
    inc si
      
    ;Interrupt to make the system wait for a certain amount of time, here less than a second  
    mov cx, 8h  ;high byte -> 1h = 1h x FFFFh microseconds
    mov ah, 86h
    int 15h
    
    jmp watchMePlay ;We get back to the start to play the next note
    
    
    
    ;Training mode, where the user must press the right key highlighted on the screen
    
    trainingMode:
    CMP [si], 0  ;We compare the current note (key code actually) of the track to zero which is marking the end of the track
    je rebootTheSystem ;We hit the zero, thus the track is over, so we reboot the system to quit the watch me play mode
    
    CMP [si], 01h
    je skipThisNote ;The 01h stands here for a pause, as a track can have pauses between some notes, so we skip that one
     
    mov ah, [si] ;We load the value of the current key code in ah at the address store in si, the one of the current key code in our array
    mov bh, ah  ;We store ah in bh to keep the value of the key that the computer expect to be pressed
    push si  ;si is going to be altered, so we put it in the stack to keep the right value
    
    ;We try to get the frequency corresponding to the current key code
    CALL defineSound
    
    push bx ;We need to keep the right value of bh, so we store bx in the stack (as we can't store 8-bits but only 16-bits values in the stack)
    mov bl, 1000b  ;We define the color of the key to be highlighted, here it's grey 
    CALL redrawKey  ;We highlight our key by redrawing it on the screen
    
    ;The key to be pressed is now shown, so we ask the user to give it a try
    mov ah, 00h
    int 16h
    
    CMP ah, 01h
    je rebootTheSystem  ;Escape, the user want to reboot and quit the training mode, thus we do so
    
    
    pop bx  ;We get back the right value of bx, and subsenquently bh
    pop si  ;We get back the right value of si
    CMP bh, ah ;ah now contains the key pressed by the user, so we compare it to bh to see if it's the expected one
    jne trainingMode ;If it's not the right one, we go on for another tour, waiting for the user to press the right key
    
    ;We push the value of si to the stack again, as it's going to be altered
    push si 
    
    ;We get the right frequency in ax, from bp where it has been stored temporarily in the defineSound procedure
    mov ax, bp  
    
    ;If we get here, it means that the right key has been pressed, so we play the sound
    CALL playSound 
    
    ;We get back the correct value of si
    pop si
    skipThisNote:
    inc si ;And we increment it to get the next key
    jmp trainingMode ;We jump back at the start to ask the user, to do the same process for the next key
    
    
    
    
    ;Procedure that take care of playing the sound according to the frequency stored in ax, and also redraw the corresponding key
    
    playSound Proc
    CMP ax, 0 
    je skip   ;If ax equals to 0 it means that the key pressed is not mapped, so we don't play anything and go right to the end of the procedure
     
    ;We set the color to brown and redraw the corresponding key with that color
    mov bl, 0110b
    CALL redrawKey                   

   
    ;We set the speaker and ask it to play our frequency
    mov     al, 182         ; Prepare the speaker for the
    out     43h, al         ;  note
          
    out     42h, al         ; Output the low byte of our frequency
    mov     al, ah          ; Output the high byte of our frequency
    out     42h, al 
    in      al, 61h         ; Turn on note (get value from
                            ;  port 61h)
    or      al, 00000011b   ; Set bits 1 and 0
    out     61h, al         ; Send new value
     
    ;We wait for a short amount of time, to let the sound play a little
    
    mov cx, 2h 
    mov ah, 86h
    int 15h 
    
    ;And we turn off the sound
    
    in      al, 61h         ; Turn off note (get value from
                            ;  port 61h).
    and     al, 11111100b   ; Reset bits 1 and 0.
    out     61h, al         ; Send new value
    
    ;As the sound has been turn off, we're going to redraw the key to its original state
                 
    CMP isABlackKey, 1
    jne whiteColor  ;It's not a black key so we're going to color it in white
     
    ;It's a black key, so we color it in black
    mov bl, 0000h
    jmp makeTheChange 
        
    whiteColor:            
    mov bl, 1111b 
    
    ;And we redraw the key with the previously passed color   
    makeTheChange:    
    CALL redrawKey 
    mov bp, 0 
    
    skip:   
    
    RET
    playSound ENDP
    
    
    ;This procedure define the sound to play, according to the key pressed 
    
    defineSound PROC 
        
    ;Check which key has been pressed
    
    CMP ah, 1Eh  ;Q  
    je playDo ;It's a do  
    
    CMP ah, 11h   ;Z
    je playDo2  ;It's a do# (1st black key)
    
    CMP ah, 1Fh  ;S
    je playRe   ;It's a re
    
    CMP ah, 12h  ;E
    je playRe2   ;It's a re# (2nd black key)
    
    CMP ah, 20h  ;D
    je playMi   ;It's a mi
    
    CMP ah, 21h  ;F
    je playFa   ;It's a fa
    
    
    CMP ah, 14h  ;T
    je playFa2  ;It's fa# (3rd black key) 
        
        
    CMP ah, 22h  ;G
    je playSol  ;It's sol
    
    
    CMP ah, 15h  ;Y
    je playSol2  ;It's sol# (4th black key)
    
    
    CMP ah, 23h  ;H
    je playLa   ;It's a la   
    
    
    CMP ah, 16h  ;U
    je playLa2   ;It's a la# (5th black key)
                  
                  
    CMP ah, 24h  ;J
    je playSi    ;It's a si
                 
    mov ax, 0           
    jmp stop ;The key pressed is not supported, so we're going to notify it with 0 in ax
    
    
    ;All of those assign the correct frequency and key graphical dimensions and coordinates, according to the one pressed
    
    playDo:
    mov ax, 4560 ; Frequency number (in decimal)
    mov bp, ax  ;We often change the value of ah, so we're going to need a second register with the value of our frequency
    mov rightSemiKeyStartAt, 12 ;The right part of the key under a black key start at the column 12
    mov leftSemiKeyEndAt, 0  ;There is no part under a black key on the left, so 0
    mov keyEndAt, 15  ;The key end at the column 15
    mov keyStartAt, 0  ;The key start at the column 0
    mov isABlackKey, 0  ;It's a white key, so 0      
    jmp stop  ;We're done here, so we move along to the end of procedure

    playDo2:
    mov ax, 4304 
    mov bp, ax 
    mov keyEndAt, 18  ;The key end at the column 18
    mov keyStartAt, 12  ;The key start at the column 12
    mov isABlackKey, 1  ;It's a black key, so 1, and therefore we don't need to set the other variables
    jmp stop  
    
    playRe:
    mov ax, 4063 
    mov bp, ax
    mov rightSemiKeyStartAt, 28 ;The right part of the key under a black key start at the column 28 
    mov leftSemiKeyEndAt, 17  ;The left part of the key under a black key end at the column 17
    mov keyEndAt, 31    ;The key end at the column 31
    mov keyStartAt, 16   ;The key start at the column 16
    mov isABlackKey, 0   ;It's a white key, so 0
    jmp stop 
    
    ;And same logic for the rest of the keys...
    
    playRe2:
    mov ax, 3834
    mov bp, ax
    mov keyEndAt, 34 
    mov keyStartAt, 28  
    mov isABlackKey, 1   
    jmp stop  

    playMi:
    mov ax, 3619
    mov bp, ax 
    mov rightSemiKeyStartAt, 48
    mov leftSemiKeyEndAt, 33
    mov keyEndAt, 47  
    mov keyStartAt, 32
    mov isABlackKey, 0   
    jmp stop  
    
    playFa:
    mov ax, 3416
    mov bp, ax 
    mov rightSemiKeyStartAt, 60
    mov leftSemiKeyEndAt, 0
    mov keyEndAt, 63  
    mov keyStartAt, 48
    mov isABlackKey, 0  
    jmp stop  
    
    playFa2:
    mov ax, 3224
    mov bp, ax
    mov keyEndAt, 66 
    mov keyStartAt, 60  
    mov isABlackKey, 1
    jmp stop  
    
    playSol:
    mov ax, 3043
    mov bp, ax   
    mov rightSemiKeyStartAt, 76
    mov leftSemiKeyEndAt, 65
    mov keyEndAt, 79  
    mov keyStartAt, 64
    mov isABlackKey, 0  
    jmp stop  
    
    playSol2:
    mov ax, 2873
    mov bp, ax
    mov keyEndAt, 82 
    mov keyStartAt, 76  
    mov isABlackKey, 1
    jmp stop 
      
    playLa:
    mov ax, 2711
    mov bp, ax 
    mov rightSemiKeyStartAt, 92
    mov leftSemiKeyEndAt, 81
    mov keyEndAt, 95  
    mov keyStartAt, 80 
    mov isABlackKey, 0 
    jmp stop
      
    playLa2:
    mov ax, 2559
    mov bp, ax 
    mov keyEndAt, 98 
    mov keyStartAt, 92  
    mov isABlackKey, 1
    jmp stop
    
    playSi:
    mov ax, 2415
    mov bp, ax 
    mov rightSemiKeyStartAt, 112
    mov leftSemiKeyEndAt, 97
    mov keyEndAt, 111  
    mov keyStartAt, 96
    mov isABlackKey, 0  
    
    
    stop: 
      
        
    RET
    defineSound ENDP
            
     
    ;This procedure take care of drawing the whole piano 
                                                 
    drawPiano PROC 
        
    mov di, 12 ;di determine the number of column we have to draw, until we reach the end of the key
    
    mov si, 0 ;Point the starting column
    
    
    ;This part take care of drawing the white keys
    
    drawWhiteColumn:  
    mov ah, 0Ch  ;This is for the interrupt, that will draw the pixel on the screen   
    mov cx, 100  ;The number of time to loop, therefore the number of lines
    mov bx, 0  ;Counter that will help us keep track of the current line
    boucle1:   
    push cx  ;We push the value of cx to the stack, as we want to keep the integrity of the loop counter store in cx
    mov al, 1111b ;White color
    mov cx, 0  
    mov cx, si ;si store the current column, so we put it in cx that indicate the column of the pixel
    mov dx, bx ;bx store the current line, so we put it in dx that indicate the line of the pixel
    add bx, 1  ;We increment bx
    int 10h   ;We draw the pixel on the screen
    pop cx ;We get back the right value of cx, to get back the integrity of our loop counter 
    loop boucle1 ;And we loop if cx is greater than 0  
    add si, 1  ;We've taken care of our column, so increment si
    CMP si, di  ;We compare si and di to know if we're done with our key
    jl drawWhiteColumn ;We're not done yet, so move on to the next column
    
    ;Are we between Mi and Fa keys ?   
    CMP di, 47 
    jne add6 ;No, so we add 6 to di, as usual, for our next black key
    
    ;Yes, so we need to notify that we're going to draw another white key        
    add si, 1  ;We add one to si in order to make a gap between the Mi and Fa keys
    add di,13  ;We add 13 to di, as the Fa key is 13 pixels large
    jmp drawWhiteColumn ;We jump back to drawing a white key, our Fa key
         
    ;Add 6 to di, in order to draw a 6 pixels large black key
    add6:
    add di, 6
       
    CMP di, 111
    jge stop2  ;We reached the end of our piano, so we end the procedure
    
    
    ;This part take care of drawing the black key and the part of the white key under them               
    
    drawBlackAndWhiteColumn:
    mov ah, 0Ch ;This is for the interrupt, that will draw the pixel on the screen  
    mov cx, 100 ;The number of time to loop, therefore the number of lines
    mov bx, 0  ;Counter that will help us keep track of the current line
    boucle4:
    CMP bx, 80     ;We compare bx to 80, which is the number of lines to "paint in black"
    jle addOneToBl  ;It's lower or equal to 80 
    jmp whitePixels ;It's greater than 80, so we draw the rest of the previous or next white key
    
    ;We add one to bx and skip the iteration to keep the pixel black, as it's the black key
    addOneToBl:
    add bx, 1
    jmp skipIteration
    
    ;We set the color to white
    whitePixels:
    mov al, 1111b ;White
    
    drawIt:
    push cx  ;We push the value of cx to the stack, as we want to keep the integrity of the loop counter store in cx 
    mov cx, 0
    mov cx, si ;si store the current column, so we put it in cx that indicate the column of the pixel 
    mov dx, bx ;bx store the current line, so we put it in dx that indicate the line of the pixel
    add bx, 1  ;We increment bx
    int 10h  ;We draw the pixel on the screen
    pop cx ;We get back the right value of cx, to get back the integrity of our loop counter
    skipIteration:  
    loop boucle4
    
    ;Those comparisons are here to let us draw the gap between the white keys
    
    CMP si, 14
    je addTwo  ;We're between Do and Re
    
    CMP si, 30
    je addTwo  ;We're between Re and Mi
    
    CMP si, 62
    je addTwo  ;We're between Fa and Sol
    
    CMP si, 78
    je addTwo  ;We're between Sol and La
    
    CMP si, 94
    je addTwo  ;We're between La and Si
    
    ;No gap so we just add one to si
    add si, 1
    jmp skipAddTwo 
    
    ;We add 2 to si, in order to make the gap between the two keys
    addTwo:
    add si, 2
    
    skipAddTwo:
    CMP si, di ;We compare si and di to know if we're done with our key
    jl drawBlackAndWhiteColumn ;We're not done yet, so we move on to the next column 
      
    ;In order to have all the keys with the same size, we need to draw 
    ;the "raw key" (the white key excluding the parts under the black keys) of Si and Mi a bit larger
    CMP di, 98 
    je add13   ;We want to draw the "raw key" of Si to be 13 pixels large
    
    CMP di, 34
    je add13  ;We want to draw the "raw key" of Mi to be 13 pixels large
    
    jmp addJust10  ;For the other keys, the "raw key" just need to be 10 pixels large
    
    add13:
    add di, 13
    jmp drawWhiteColumn
         
    addJust10:
    add di, 10
    
    
    CMP di, 111
    jle drawWhiteColumn ;We make sure we didn't reach the end of the piano before continuing
    
    
    stop2:
                  
    RET
    drawPiano ENDP    
    
    
    ;This procedure redraw the key to highlight it when the user press it or to show which key to press in watch me play mode
    
    redrawKey PROC 
        
    CMP isABlackKey, 1  ;We check if we want to redraw a black key
    je redrawBlackKey  ;It's a black key, so we jump to the proper part to handle that
    
    
    ;This part redraw a whole white key
    
    redrawWhiteKey: 
    mov si, keyStartAt ;We indicate to si where to start
    mov dx, 0  ;We always start at the line 0
    mov al, bl ;We pass to al the color, in which to draw the key, that has been stored to bl
    drawColumnWhiteKey: 
    mov ah, 0Ch   
    mov cx, 100 
    mov bx, 0
    boucleWhiteKey:
    CMP si, rightSemiKeyStartAt  
    jge semiWhiteKey ;We are in the right part of the key where it's under a black key 
    
    CMP leftSemiKeyEndAt, 0 
    je colorTheWhiteKey  
    ;As we can't compare it with si when it's equal to zero for the first key,
    ;we do a special comparison for it
    
    CMP si, leftSemiKeyEndAt 
    jle semiWhiteKey ;We are in the left part of the key where it's under a black key
    
    jmp colorTheWhiteKey ;We're not under any black key, so we go on drawing our white key normally
    
    semiWhiteKey:
    CMP bx, 80
    jle addOneToBl2 
    ;If we're here it means that we are under a black key, so if we're currently in the black key,
    ;we need to do something special
    jmp colorTheWhiteKey ;We're in the white key, so we carry on drawing our white key
    
    ;We're in our black key, so we're got nothing to redraw, thus we add one to bx and skip that iteration
    addOneToBl2:
    add bx, 1
    jmp skipIteration2 
     
    colorTheWhiteKey:
    ;We already detailed that process several times above
    push cx 
    mov cx, 0
    mov cx, si 
    mov dx, bx
    add bx, 1  
    int 10h
    pop cx   
    skipIteration2:
    loop boucleWhiteKey   
    add si, 1
    CMP si, keyEndAt ;We compare si to the end of the key
    jl drawColumnWhiteKey ;We're not done with our key, so we move on to the next column
    
    jmp stopit
    
     
    ;This part redraw a black key
    
    redrawBlackKey: 
    mov si, keyStartAt  ;We indicate to si where to start
    mov dx, 0  ;We always start at the line 0
    mov al, bl ;We pass to al the color in which to draw the key that has been stored to bl
    drawColumnBlackKey: 
    mov ah, 0Ch   
    mov cx, 81  ;The counter is equal to 81 here, as we're redrawing only the black key, and not the part of the white keys below
    mov bx, 0
    boucleBlackKey: 
    ;We already detailed that process several times above
    push cx 
    mov cx, 0
    mov cx, si 
    mov dx, bx
    add bx, 1  
    int 10h
    pop cx   
    loop boucleBlackKey   
    add si, 1
    CMP si, keyEndAt ;We compare si to the end of the key
    jl drawColumnBlackKey ;We're not done with our key, so we move on to the next column
    
    stopit:
    mov ax, bp 
    ;We changed the value of ax by using al and ah, so we restore it by getting it back from bp
    ;where it has been stored temporarily
    
    
    RET
    redrawKey ENDP  
    
    ;This procedure print the message whose address has been passed into si
          
    printString PROC
    ;We're going to edit ax and si, so we add their values to the stack
    push    ax      
    push    si      
    nextChar:      
            mov     al, [si]  ;si represent the address of the current character to print, so we put its value in al
            CMP     al, 0   ;0 indicate the end of our string, so we check if we have reached its end or not
            je      printed  ;We reach the end of the string, so we quit the procedure
            inc     si   ;We increment si as we already moved the value of our character to al
            mov     ah, 0eh  ;For our interrupt which is going to print our character
            int     10h  ;We print the character store in al
            jmp     nextChar ;And we jump to the next character
    printed:
    ;We get the original value of si and ax from the stack
    pop     si      
    pop     ax
          
    RET
    printString ENDP 
    
    
    ;Procedure that clear the window the easy way by redefining the video text mode
    clearWindow PROC
    mov ah, 00h
    mov al, 03h
    int 10h 
        
    RET    
    clearWindow ENDP
    
    
    ;This procedure load the right track according to the choice of the user
    loadTheMusic PROC
    CMP ah, 02h
    je loadTrack1 ;First track chosen
    
    CMP ah, 03h
    je loadTrack2  ;Second track chosen
    
    CMP ah, 04h
    je loadTrack3  ;Third track chosen
    
    jmp done   ;We should never hit that line, but if we do, we don't load anything
    
    loadTrack1:
    lea si, track1  ;We load the address of 'Au clair de la lune' in si
    jmp done
    
    loadTrack2:
    lea si, track2  ;We load the address of 'Do, re, mi, fa, sol, la, si...' in si
    jmp done
    
    loadTrack3:
    lea si, track3  ;We load the address of 'Jingle bells' in si
    
    done:        
    RET
    loadTheMusic ENDP 
    
    
    ;This procedure take care of setting the piano
    
    getThePianoReady PROC
     
    ;We're going to edit ax and si, so we add their values to the stack  
    push ax
    push si
    
    ;We set the graphical video mode      
    mov ah, 00h
    mov al, 13h
    int 10h      
    
    ;We call the procedure to draw our piano
    CALL drawPiano
    
    ;We get the original value of si and ax from the stack
    pop si
    pop ax
    
    RET
    getThePianoReady ENDP
    
    
    ret 
    
    ;Variables that store the beginning and end of each key to redraw when pressed
       
    keyStartAt dw 0  ;Store the beginning column of a key
    keyEndAt dw 0   ;Store the end column of a key
    leftSemiKeyEndAt dw 0  ;Store the end column of the left part of a white key under a black key
    rightSemiKeyStartAt dw 0  ;Store the beginning column of the right part of a white key under a black key
    isABlackKey db 0  ;Store if a key is a black key or not
    
    
    ;Store the welcome message with all the option at the launch of our program    
    msg  db "Welcome to Piano ASM",0Dh,0Ah   
         db "What do you want to do ?",0Dh,0Ah 
         db "1 - Free to play",0Dh,0Ah
         db "2 - Watch me play",0Dh,0Ah
         db "3 - Training mode",0Dh,0Ah  
         db "4 - Help",0Dh,0Ah 
         db "> ",0Dh,0Ah, 0 
         
    
    ;Store a message to indicate the user that he has to enter one of the correct input suggested     
    tryAgainMsg db "Please enter one of the choices below",0Dh,0Ah 
                db " ", 0Dh,0Ah, 0   
                
    
    ;Store the message for the menu of the watch me play mode            
    watchMePlaySubMenuMsg db "Which music do you want the computer to play ?",0Dh,0Ah
                          db "1 - Au clair de la lune",0Dh,0Ah 
                          db "2 - Do, re, mi, fa, sol, la, si...",0Dh,0Ah 
                          db "3 - Jingle bells",0Dh,0Ah 
                          db "> ",0Dh,0Ah, 0 
    
    ;Store the message for the menu of the training mode                     
    trainingSubMenuMsg db "Which music do you want to train on ?",0Dh,0Ah
                       db "1 - Au clair de la lune",0Dh,0Ah
                       db "2 - Do, re, mi, fa, sol, la, si...",0Dh,0Ah
                       db "3 - Jingle bells",0Dh,0Ah 
                       db "> ",0Dh,0Ah, 0
    
    ;Store the message for the help page                  
    helpMsg db  "Q: How the piano works?",0Dh,0Ah
            db  "A: The keys are mapped with the following pattern (on an AZERTY keyboard):",0Dh,0Ah
            db  "    ___________________________",0Dh,0Ah
            db  "   |  | | | |  |  | | | | | |  | ",0Dh,0Ah
            db  "   |  | | | |  |  | | | | | |  |",0Dh,0Ah
            db  "   |  |Z| |E|  |  |T| |Y| |U|  | ",0Dh,0Ah  
            db  "   |  |_| |_|  |  |_| |_| |_|  | ",0Dh,0Ah  
            db  "   |   |   |   |   |   |   |   | ",0Dh,0Ah
            db  "   | Q | S | D | F | G | H | J | ",0Dh,0Ah  
            db  "   |___|___|___|___|___|___|___| ",0Dh,0Ah  
            db  "",0Dh,0Ah
            db  "Q: What are the frequencies used?",0Dh,0Ah
            db  "A: The piano starts with the Middle C and displays one octave: ",0Dh,0Ah 
            db  "   C4 to B4 including the '#' ones.",0Dh,0Ah
            db  "",0Dh,0Ah 
            db  "Q: How to come back to the menu?",0Dh,0Ah
            db  "A: Just press 'ESC'.",0Dh,0Ah, 0    
         
    
    ;Au clair de la lune (01h stands for a pause as it's not mapped to any frequency)     
    track1 db 1Eh, 1Eh, 1Eh, 1Fh, 20h, 01h, 1Fh, 01h, 1Eh, 20h, 1Fh, 1Fh, 1Eh, 01h, 01h
           db 1Fh, 1Fh, 1Fh, 1Fh, 23h, 01h, 23h, 01h, 1Fh, 1Eh, 24h, 23h, 22h, 01h, 01h
           db 1Eh, 1Eh, 1Eh, 1Fh, 20h, 01h, 1Fh, 01h, 1Eh, 20h, 1Fh, 1Fh, 1Eh, 0
           
    ;Do, re, mi, fa, sol, la, si...
    track2 db 1Eh, 1Fh, 20h, 21h, 22h, 23h, 24h
           db 16h, 15h, 14h, 12h, 11h, 0
           
    
    ;Jingles Bells 
    track3 db 20h, 20h, 20h, 01h, 20h, 20h, 20h, 01h, 20h, 22h, 1Eh, 1Fh
           db 20h, 01h, 01h, 21h, 21h, 21h, 01h, 21h, 21h, 01h, 20h, 20h, 20h, 20h
           db 20h, 1Fh, 1Fh, 20h, 1Fh, 01h, 22h, 01h, 20h, 20h, 20h, 01h, 20h, 20h, 20h, 01h
           db 20h, 22h, 1Eh, 01h, 1Fh, 20h, 01h, 01h, 21h, 21h, 21h, 21h, 01h
           db 21h, 01h, 20h, 20h, 01h, 20h, 20h, 01h, 22h, 22h, 01h, 21h, 1Fh, 1Eh, 0   
                                           