Program DiskTest;
 
{$M 8192, 131072, 131072}
{$N+ enable FPU, for scaling of output on pattern tests}
{$E+ enable FPU emulation if FPU not present}
 
 
{IOMeter type performance tests for XT/AT PCs with MS-DOS.
 Used for the development of the Dangerous Prototype XT-IDE board, and
 subsequently the lo-tech XT-CF board.
 
 Includes pattern tests for testing 8-bit interface reliability and
 to generate patterns to check with a scope attached.
 
 
 
 Performance Testing
 ===================
 
 By default, creates a 4MB file in the current working directory using
 32K IOs.  Then reads it back also using 32K IOs.
 
 Next performs two random workload tests on it:
 
 - 70% read, using 8K IOs (8K aligned)
 - 100% read, using 512-byte IOs (sector aligned)
 
 Finally displays the results, with speed calculated based on the system
 clock.  Note therefore that any hardware causing clock-skew via extended
 disabling of interupts (causing loss of timer ticks) will cause results
 to be overstated.
 
 
 Pattern Testing
 ===============
 
 For media testing, 'mediatest' command line option runs a 10-pass
 pattern test.  Default file size is 4MB; this can be extended to all
 available free space by also specifying 'maxsize'.  As with the performance
 test, test size will be automatically truncated to available disk space.
 
 Tests are potentially time consuming - can skip on (to the next part) by
 pressing any key.  Progress is shown onscreen though.
 
 
 For interface testing, special patterns can be tested using the command
 line option 'signaltest'.  This option runs patterns that enable
 assessment of signal quality with an osciloscope:
 
 Test 1 - For testing at DD7.  Flips the bit continually, all others
          will be low.  Line DD7 has a 10k pull-down at the interface.
 
 Test 2 - For testing at DD11.  Holds the bit low and flips all other
          bits continually.  Enables measurement of cross-talk as the
          line serving this bit is in the middle of the data lines on
          the 40-pin connector.
 
 Test 3 - For testing on the ISA Bus at data bit 4 (ISA slot pin A5).
          To enable assessment of signal quality on the ISA bus.
          Flips this bit repeatedly.
 
 
 By James Pearce, lo-tech.co.uk
 Modified by Foone Turing, foone.org
 
 v1.0 - 06-May-10
        - Initial build 32K read and write and 8k random
 
 v1.1 - 15-Apr-11
        - Fixed transfer sizes (were 512-bytes too large)
 
 v1.2 - 26-Apr-11:
        - Added 512-byte random read test
          - note random r-w at sector level would be misleading due
            to variable DOS cluster sizes.  Writes less than a cluster
            must be performed as read-update-write.
        - Added automatic reduction of test file size depending on free
          space available.
        - Added command line options (any order):
          - 'maxsize', to test with maximum free space of drive
          - 'maxseeks', to test random with 4096 seeks (default is 256)
          - 'highseeks', to test random with 1024 seeks
          - 'lowseeks', to test random with 128 seeks
          - 'minseeks', to test random with 32 seeks (for floppys)
          - '-h' or '/h' or '/?' to display a command line reference
 
 v1.3 - 08-Sep-11:
        - Added 'testintegrity' option, which performs extended pattern
          testing and reports on any errors encountered (instead of the
          performance tests).
 
 v1.4 - 10-Sep-11:
        - Revised checking method to detect errors for integrity test
        - Added run-time and error notification to end of test
        - Increased to 10 patterns
        - Added progress indicators and some formating to ensure entire
          test info will always show on one screen, regardless of test
          file size.
        - Made pattern tests 'two dimensional', to test interface integrity
          too (i.e. individual bits in consecutive words transferred will
          change)
        - Added 'crosstalktest' option (for XT-IDE prototype scoping, see
          below)
 
 v1.5 - 14-Sep-11:
        - Renamed and extended pattern tests
        - Revised most parts for efficiency
        - Set memory limits to ensure will run on 256K XT
        - consolidated pattern test code
        - added option 'cycle' to shift bits for every word on media-test
 
 v1.6 - 19-Sep-11:
        - Changed pattern test procedure to a function (returns no. of errors)
        - Revised display scaling code in pattern test
        - Added operational indication to performance tests
        - Added ability to completely terminate pattern tests (press 'Q')
 
 v1.7 - 14-Feb-12:
        - Added "size=" parameter to enable test size to be directly set.
          Specified size can include K or M for KB or MB respecitively.
          Specified size must be >64KB, and will be truncated to avaulable
          disk space.  E.g: size=8M will define an 8MB test file, if that
          much space is available.
 
 v1.8 - 16-Feb-12:
        - Added 'powertest' to signal test function and refined pattern
          writing utility to support looping operation (to support it).
 
          The powertest mode continually reads or writes 0x55AA 0xAA55 pairs
          as fast as possible (without verifying), simply wrapping at the end
          of the test file size allocated repeatedly until the test is
          terminated by the user.
 
          By flipping all 16-bits on every word, and all 8-bits on every
          byte, power consumption of the card should be maximised, hence
          allowing measurement of power drawn of the card in this worst-case
          test pattern.
 
        - Re-designed help screens as too much for one 80x25 page
 
v1.9 - 10-Apr-12
	- Changed pattern definitions to consider the two seperate 8-bit transfers
	  more closely.  Patterns are now:
	  0,$FFFF,$F0F0,$CCCC,$AAAA,$AA55,$A5A5,$FF00,$18E7,$E718
 
v2.0 - 29-Apr-12
        - Added readonly test option.
 
v2.1 - 30-Apr-12
	- Re-coded block comparison function in pattern tests; testing
	  now runs *much* faster on XT class hardware (5x).
 
v2.2 - 01-May-12
	- Changed pattern testing to include both static patterns and
          walking-ones / walking-zeros tests.  Basic patterns are now:
	  0,$FFFF,$FF00,$F00F,$AA55,$A55A,$18E7,$E718, then
          walking-1s and walking-0s.
	  Cycle option has been removed.
 
v2.3 - 04-May-12
	- Added RAM test to the pattern test routine.
	- Added 'noprogress' switch to surpress screen progress marks on
	  performance tests.

v2.4 - 04-Oct-17
  - Delete test file at end of test
}
 
 
Uses Dos, Crt;
 
 
TYPE
  Sector            = array[0..255] of word; {512 bytes}
 
  T32kArray         = array[0..16383] of word; {32KB}
  P32kArray         = ^T32kArray;
 
  T8kArray          = array[0..8191] of word; {8k}
  P8kArray          = ^T8kArray;
 
 
CONST
  TestSize          : LongInt = 4194304; {4MB}
  FName             : String = 'TEST$$$.FIL';
  Seeks             : Word = 256;
  PatternTests      : Byte = 10;
  Patterns          : Array[1..10] of Word =
                      (0,$FFFF,$FF00,$F00F,$AA55,$A55A,$18E7,$E718,$0001,$FFFE);
  PatternCycle      : Array[1..10] of Byte =
                      (0,    0,    0,    0,    0,    0,    0,    0,    1,    1);
  PatternNames      : Array[1..10] of String[10] =
                      ('','','','','','','','','Walking 1s','Walking 0s');
  PowerPatterns     : Array[1..2] of Word = ($55AA,$AA55);
  VERSION           : String = '2.4';
  DisplayCodesCount : Byte = 4;
  DisplayCodes      : Array[1..4] of Char = ('-','\','|','/');
 
  {Pattern test writer modes}
  PatRead           : Byte = 1;
  PatReadContinuous : Byte = 2;
  PatWrite          : Byte = 4;
  PatWriteContinuous : Byte = 8;
  PatVerify         : Byte = 16;
  PatPrompt         : Byte = 32;
 
VAR
  Time              : Real;
  QUIT              : Boolean;
  noprogress        : Boolean;
 
 
procedure StartClock;
var
  hr, min, sec, sec100 : word;
begin
  GetTime(hr,min,sec,sec100);
  Time := (hr * 3600) + (min * 60) + sec + (sec100 / 100);
end;{StartClock}
 
 
function StopClock : Real;
var
  hr, min, sec, sec100 : word;
  Time2 : real;
begin
  GetTime(hr,min,sec,sec100);
  Time2 := (hr * 3600) + (min * 60) + sec + (sec100 / 100);
  If Time2 = Time then
    StopClock := 0.01 {prevent div by 0}
  else
    StopClock := (Time2 - Time);
end;{StopClock}
 
 
 
function CreateFile : Real;
{creates the file and returns the rate in KB/s}
var
  f           :  file of T32kArray;
  buffer      :  P32kArray;
  i           :  word;
  max         :  word;
  res         :  word;
  Mark, X, Y  :  Byte;
 
begin
  Assign(f,FName);
  Rewrite(f);
  new(buffer);
  max := TestSize div SizeOf(T32kArray);
  X := WhereX; Y := WhereY;
  Mark := 1;
 
  StartClock;
 
  {now do the write test}
  if noprogress then
  begin
    for i := 1 to max do
      write(f,buffer^);
  end else begin
    for i := 1 to max do
    begin
      Inc(Mark);
      If Mark > DisplayCodesCount then Mark := 1;
      write(DisplayCodes[Mark]);
      GotoXY(X,Y);
      write(f,buffer^);
    end;{for}
  end;{if/else}
 
  close(f);
  CreateFile := TestSize / 1024 / StopClock;
  Dispose(buffer);
end;{createfile}
 
 
 
function ReadFile : Real;
{reads the file and returns the rate in KB/s}
var
  f           :  file of T32kArray;
  buffer      :  P32kArray;
  i           :  word;
  max         :  word;
  res         :  word;
  Mark, X, Y  :  Byte;
 
begin
  Assign(f,FName);
  Filemode := 0; {open read-only}
  Reset(f);
  new(buffer);
  max := TestSize div SizeOf(T32kArray);
  X := WhereX; Y := WhereY;
  Mark := 1;
 
  StartClock;
 
  {now do the read test}
  if noprogress then
  begin
    for i := 1 to max do
      read(f,buffer^);
  end else begin
    for i := 1 to max do
    begin
      Inc(Mark);
      If Mark > DisplayCodesCount then Mark := 1;
      write(DisplayCodes[Mark]);
      GotoXY(X,Y);
      read(f,buffer^);
    end;{for i}
  end;{if/else}
 
  close(f);
  ReadFile := TestSize / 1024 / StopClock;
  dispose(buffer);
end;{createfile}
 
 
 
function RandomTest( transfersize : word; readpercent : byte ) : Real;
{Random IOPS test, returns the rate in IOPS
 Tests IOs of up to 32K each - transfersize is the size in bytes
 readpercent is percentage that should be reads (50 = 50%)}
 
type
  PositionArray = array[1..4096] of LongInt;
 
var
  f          :  file;
  buffer     :  P32kArray;
  i, n       :  word;
  max        :  LongInt;
  p1, p2     :  word;
  max1, max2 :  word;
  res        :  word;
  p          :  LongInt;
  Positions  :  ^PositionArray;
  limit      :  byte;
  Mark, X, Y  :  Byte;
  TimeInSeeks,
  TimeInTransfers : Real;
 
begin
  Randomize;
  Assign(f,FName);
  if readpercent = 100 then
    Filemode := 0 {open read-only}
  else Filemode := 2; {open read/write}
  Reset(f,1);
  max  := TestSize - transfersize; {take off the transfer size to allow for
                                    the last block}
  max1 := max SHR 16;
  max2 := max AND 65535;
  p1 := 0;
 
  new(Positions);
  new(buffer);
 
  for i := 1 to Seeks do begin
    {create an array of seek positions (in bytes)}
    if max1 > 0 then p1 := Random(max1);
    p2 := Random(max2);
    p  := (longint(p1) SHL 16) + (p2 AND $FE00); {sector align}
    Positions^[i] := p;
  end;{for}
 
  n := 1;
  X := WhereX; Y := WhereY;
  Mark := 1;
  limit := readpercent div 10;
 
  StartClock;
  TimeInSeeks := 0; TimeInTransfers := 0;
 
  {do the random IO test}
  for i := 1 to Seeks do begin
    if not noprogress then
    begin
      Inc(Mark);
      If Mark > DisplayCodesCount then Mark := 1;
      write(DisplayCodes[Mark]);
      GotoXY(X,Y);
    end;{if not noprogress}
    seek(f,Positions^[i]);
    {n keeps track of reads and writes}
    if n <= limit then
      blockread(f,buffer^,transfersize)
    else
      blockwrite(f,buffer^, transfersize,res);
    inc(n);
    if n > 10 then n := 1;
  end;{for}
 
  close(f);
  RandomTest := i / StopClock;
  Dispose(buffer);
  Dispose(Positions);
end;{RandomTest}
 
 
 
procedure PurgeTestFile;
{purges the contents of the test file, if it's present}
var f : file;
begin
  Assign(f,FName);
  Rewrite(f);
  Truncate(f);
  Close(f);
end;{procedure PurgeTestFile}

procedure DeleteTestFile;
{deletes the test file}
var f : file;
begin
  WriteLn('Deleting ', FName, '.');
  Assign(f,FName);
  Erase(f);
end;{procedure DeleteTestFile}
 
 
function CheckTestFile : LongInt;
{returns the size of the test file, if it can be found}
var f : file;
begin
  Assign(f,FName);
  Filemode := 0; {open readonly}
  {$I-}
  Reset(f,1);
  {$I+}
  If IOResult <> 0 then
    CheckTestFile := 0
  else begin
    CheckTestFile := FileSize(f);
    Close(f);
  end;{if IOResult}
end;{function}
 
 
Function COMPSW( source, destination : pointer; words : word) : Word;
{implements COMPSW function, comparing source with destination on a
 word-by-word basis, returning zero if they were OK}
var
  rDS, rSI,
  rES, rDI	  :  Word;
  BlockBad	  :  Word;
begin
  rDS := Seg(source^);
  rSI := Ofs(source^);
  rES := Seg(destination^);
  rDI := Ofs(destination^);
 
  asm
    push	ds
    push	si
    push	es
    push	di
 
    mov		ax, rDS
    mov		ds, ax
    mov		ax, rSI
    mov		si, ax
 
    mov		ax, rES
    mov		es, ax
    mov		ax, rDI
    mov		di, rDI
 
    mov		cx, words
    cld
 
    rep		cmpsw
    jz		@NoDiffs
 
    inc		cx
 
  @NoDiffs:
    mov		BlockBad, cx
 
    pop	di
    pop	es
    pop	si
    pop	ds
  end;{asm}
 
  COMPSW := BlockBad;
end;{function COMPSW}
 
 
Function PatternTest( WriteBlock, Rd : P32kArray; DisplayStr : string;
                      Mode : Byte ) : LongInt;
{writer for pattern testing.  Returns the number of errors encountered.
 See constants at the top for the mode options (which can be combined).
 Also checks the two transfer buffers with the pattern in question first,
 to avoid showing a RAM problem as a controller problem.}
var
  f               :  file of T32kArray;
  ErrCount,
  TotalErrors     :  LongInt;
  IO, j, i        :  Word;
  max, readmax    :  LongInt;
  Dots, Mark, Next,
  CurrentDot      :  Byte;
  CurrentPosition :  Single;
  X, Y, XBreak,
  X1, X2, Y1      :  Byte;
  Ch              :  Char;
  PromptStr       :  String;
 
begin
  Assign(f,FName);
  Filemode := 2;{read/write}
 
  max := TestSize div SizeOf(T32KArray); {number of IOs for test}
  readmax := max; {size of read test - will reduce if write phase is skipped}
 
  TotalErrors := 0;
  ErrCount := 0;
  DisplayStr := DisplayStr + ' - Writing: ';
  Dots := (78 - Length(DisplayStr) - Length(' Comparing: ')) div 2;
    {Dots will be the number of dots to displayed during the test each way}
 
  {write out info + get cursor position}
  Write(DisplayStr);
  X := WhereX; Y := WhereY;
  X1 := X; Y1 := Y;
  Ch := CHAR(0);
 
  {now write out file - will always do this part}
  repeat
    GoToXY(X1,Y1);
    for i := 1 to dots do write(' ');
    GoToXY(X1,Y1);
    X := X1;
    Reset(f);
    CurrentDot := 0;
    Mark := 1;
    for IO := 1 to max do
    begin
      {calculate when to write a dot on screen.}
      CurrentPosition := IO * Dots / max;
 
      {debug line follows - introduces an error on one 32K block written}
      {if IO = 4 then write(f,Rd^) else}
      write(f,WriteBlock^);
 
      {now update screen}
      Next := trunc(CurrentPosition);
      if (Next > CurrentDot) then
      begin
        {time to advance dot(s) on screen}
        repeat
          Write('.');
          Inc(X);
          Inc(CurrentDot);
        until CurrentDot = Next;
      end else begin
        Inc(Mark);
        If Mark > DisplayCodesCount then Mark := 1;
        write(DisplayCodes[Mark]);
        GotoXY(X,Y);
      end;
      if KeyPressed then begin
        while KeyPressed do Ch := ReadKey; {clear keyboard buffer}
        if (Ch = ' ') and ((Mode AND PatWriteContinuous) = PatWriteContinuous) then
          {space finishes the current write block, then skips}
          Mode := Mode AND NOT PatWriteContinuous
        else begin
          readmax := IO;  {record how many blocks were written}
          IO := max;      {end write loop}
          if (readmax < max) then
          begin
            if X > (80 - Dots - Length(' Comparing: ') - Length('Skipped ')) then
              GotoXY((80 - Dots - Length(' Comparing: ') - Length('Skipped ')),Y);
            Write('Skipped ');
          end;{if (readmax<max)}
          If UpCase(Ch) = 'S' then readmax := 0
          else if UpCase(Ch) = 'Q' then begin
            QUIT := true;
            readmax := 0;
          end;{if/else}
        end;{if Ch=' '}
      end;{if keypressed}
    end;
  until (Ch<>CHAR(0)) OR ((Mode AND PatWriteContinuous) <> PatWriteContinuous);
 
  If ((Mode AND PatPrompt) = PatPrompt) then
  begin
    if ((Mode AND PatVerify) = PatVerify) then
      PromptStr := 'Verify '
    else
      PromptStr := 'Read ';
    if ((Mode AND PatReadContinuous) = PatReadContinuous) then
      PromptStr := PromptStr + '(C)ontinuous/';
    PromptStr := PromptStr + '(O)nce/(N)o: ';
    X2 := 78 - Dots - Length(PromptStr);
    GotoXY( X2, Y );
    Write(PromptStr);
    If ((Mode AND PatReadContinuous) = PatReadContinuous) then
      repeat
        Ch := Upcase(ReadKey);
      until Ch in ['C', 'O', 'N']
    else
      repeat
        Ch := Upcase(ReadKey);
      until Ch in ['O', 'N'];
    case Ch of
      'O' : mode := mode AND NOT PatReadContinuous; {user selected not continuous}
      'N' : mode := 0; {nothing to do}
    end;{case}
    GotoXY( X2, Y );
    for i := 1 to length(PromptStr) do
      Write(' ');
  end;{read and verify behaviour required check}
 
  while keypressed do Ch := ReadKey; {clear keyboard buffer}
  Ch := CHAR(0); {clear input}
 
  while (Ch = CHAR(0)) AND (readmax > 0) AND ((mode AND PatRead) = PatRead) do
  begin
    {now read back and compare if verify was specified}
    GotoXY( (78 - Dots - Length('Comparing: ')), Y );
    if ((mode and PatVerify) = PatVerify) then
         Write(' Comparing: ')
    else Write('   Reading: ');
    reset(f);
 
    CurrentDot := 0;
    X := WhereX;
    for IO := 1 to readmax do
    begin
      {calculate when to write a dot on screen}
      CurrentPosition := IO * Dots / readmax;
 
      {read the block}
      read(f,Rd^);
 
      {find which dot we're on now}
      Next := trunc(CurrentPosition);
 
      {compare what was read with what was written, if we need to}
      if ((mode AND PatVerify) = PatVerify) then
        ErrCount := ErrCount + COMPSW(WriteBlock,Rd,16384);
 
      {now update screen}
      if (Next > CurrentDot) then
      begin
        {time to advance mark(s) on screen}
        repeat
          if ErrCount = 0 then Write('û')
          else write('!');
          Inc(X);
          Inc(CurrentDot);
        until CurrentDot = Next;
 
        if ErrCount <> 0 then
        begin
          Inc(TotalErrors);
          ErrCount := 0;
        end;
      end else begin
        Inc(Mark);
        If Mark > DisplayCodesCount then Mark := 1;
        write(DisplayCodes[Mark]);
        GotoXY(X,Y);
      end;
 
      {check if user has interupted}
      if KeyPressed then begin
        while KeyPressed do Ch := ReadKey; {clear keyboard buffer}
        IO := readmax;      {end read loop}
        if X > (79 - Length('Skipped')) then
          GotoXY((79 - Length('Skipped')),Y);
        Write('Skipped');
        if UpCase(Ch) = 'Q' then QUIT := true;
      end;{if}
    end;
    If ((mode AND PatReadContinuous) <> PatReadContinuous) then
      mode := mode XOR PatRead; {end while loop as only one pass was required}
  end;{while...}
 
  {clear up}
  WriteLn(' ');
  close(f);
 
  PatternTest := TotalErrors;
end;{procedure PatternTest}
 
 
 
function InHex(value : word) : string;
var
  i, num      :   byte;
  Ch          :   char;
 
begin
  InHex := '0x';
  InHex[0] := char(6); {set length to 6 chars}
  for i := 1 to 4 do begin
    num := value AND 15;   {lower 4 bits}
    value := value SHR 4; {strip off lower for, for the next iteration}
    if num < 10 then ch := char(num+48) {'0' is ASCII 48}
    else ch := char(num+55); {'A' is ASCII 65}
    InHex[(7-i)] := ch;
  end;{for}
end;{fucntion}
 
 
function TwoDigit(var number : byte) : String;
var
  TempStr : String;
begin
  Str(number,TempStr);
  if number < 10 then TwoDigit := '0' + TempStr
  else TwoDigit := TempStr;
end;{function}
 
 
procedure MediaTest;
var
  Wr, Rd      :  P32kArray; {we allocation both memory blocks here,}
  Test        :  byte;      {so that we can test the pattern in RAM first}
  DisplayStr  :  String;
  H, M, S     :  Byte;
  TestTime    :  Real;
  i           :  Word;
  Errors      :  LongInt;
  Res         :  LongInt;
  WrDS, WrSI  :  Word;
 
begin
  Write('Pattern testing with ',PatternTests,' patterns over ');
  if (TestSize > 1048576) then
    {file is MB size}
    write( (TestSize / 1048576):1:1,' MB.')
  else
    {file is KB size}
    write( (TestSize div 1024),' KB.');
  WriteLn;
  Write('Press any key to skip on, S to skip test completely, Q to quit.');
  WriteLn; WriteLn;
 
  new(Wr); new(Rd);
  Errors := 0;
  QUIT := false;
 
  StartClock;
 
  for Test := 1 to PatternTests do
  begin
    {fill array with pattern}
    if (PatternCycle[Test]=1) then begin
      {this is a cyclic test, i.e. walking 1's or walking 0's etc}
      {build the pattern array shifting every BYTE, since it's an 8-bit interface}
      {we're testing}
      WrDS := Seg(Wr^); WrSI := Ofs(Wr^);
      Wr^[0] := Patterns[Test];
      asm
        push    ds
        push    si
        push	ax
        push	cx
 
        mov     ds, WrDS
        mov     si, WrSI
        mov	ax, ds:[si]
        mov	cx, 32768
 
	@comploop:
	mov	ds:[si], al
	rol	al, 1
        inc     si
	loop	@comploop
 
        pop	cx
        pop	ax
        pop     si
        pop     ds
      end;{asm}
    end else
      for i := 0 to 16383 do Wr^[i] := Patterns[Test]; {fill array with pattern}
 
    {now check the allocated RAM is free from apparent errors}
    Rd^ := Wr^; {copy between the buffers}
 
    {get test name}
    If (PatternCycle[Test]=1) then
      DisplayStr := PatternNames[Test]
    else
      DisplayStr := 'Pattern ' + InHex(Patterns[Test]);
 
    {check the allocated RAM blocks for errors}
    if COMPSW(Wr,Rd,16384) <> 0 then
    begin
      {RAM apparently is bad}
      WriteLn('RAM Error detected with ', DisplayStr, '.');
      WriteLn('  Block A is at: ', InHex(Seg(Wr^)), ':', InHex(Ofs(Wr^)), 'h');
      WriteLn('  Block B is at: ', InHex(Seg(Rd^)), ':', InHex(Ofs(Rd^)), 'h');
      WriteLn('  Source is at : ', InHex(Seg(Patterns)), ':', InHex(Ofs(Patterns)+Test), 'h'); 
      WriteLn; Write('Checking: ');
      for i := 0 to 16383 do begin
        if Wr^[i] <> Rd^[i] then begin
          WriteLn('Difference encountered at offset ', InHex((i*2)));
          i := 16383; {drop out of loop}
        end;
      end;{for}
      dispose(Rd); dispose(Wr);
      Exit;
    end;{if COMPSW}
 
    {Otherwise, run the pattern test}
    Errors := Errors + PatternTest(Wr,Rd,DisplayStr,(PatRead+PatWrite+PatVerify));
    if Quit then Test := PatternTests;
  end;{for}
 
  TestTime := StopClock;
  Dispose(Rd); Dispose(Wr);
 
  H := Trunc(TestTime) div 3600;
  M := (Trunc(TestTime) mod 3600) div 60;
  S := Trunc(TestTime) mod 60;
 
  WriteLn;
  Write('Test ran for ',TwoDigit(H),':',TwoDigit(M),':',TwoDigit(S),'.  ');
  If Errors = 0 then Write('No')
  else write(Abs(Errors),' 32K');
  write(' blocks had errors.');
end;{procedure MediaTest}
 
 
 
procedure SignalTest;
{optional pattern tests specifically targetted at 8-bit XT/IDE adapter
 development.  See info in narrative at top of file.}
var
  Test        :  Byte;
  Wr, Rd      :  P32kArray;
  EndOfTest   :  Boolean;
  Ch          :  Char;
  i           :  Word;
  DisplayStr  :  String;
  Errors      :  LongInt;
  TestMode    :  Byte;
 
begin
  WriteLn('XT/IDE Development Pattern Tests - using ',(TestSize div 1048576),'MB test file.');
 
  New(Wr); New(Rd);
  EndOfTest := False;
  Errors := 0;
 
  repeat
    WriteLn;
    WriteLn('Test 1 - For testing at DD7.  Flips the bit continually, all others');
    WriteLn('         will be low.  Line DD7 has a 10k pull-down at the interface.');
    WriteLn;
    WriteLn('Test 2 - For testing at DD11.  Holds the bit low and flips all other bits');
    WriteLn('         continually.  Enables measurement of cross-talk as the line serving');
    WriteLn('         this bit is in the middle of the data lines on the 40-pin connector.');
    WriteLn;
    WriteLn('Test 3 - For testing on the ISA Bus at data bit 4 (ISA slot pin A5).  To enable');
    WriteLn('         assessment of ISA bus signal quality, flips this bit repeatedly.');
    WriteLn;
    WriteLn('Test 4 - For measuring peak power consumption of the interface under read and');
    WriteLn('         write workloads.  Total power consumption will be affected by the');
    WriteLn('         system (and bus) speed, since faster switching will use more power.');
    WriteLn('         Test patterns are', InHex(PowerPatterns[1]), ' and ', InHex(PowerPatterns[2]), '.' );
    WriteLn;
    WriteLn('Test 5 - As test 4, except that the read part of the test is a one-pass verify');
    WriteLn('         This will run much slower, but will confirm, after a heavy write test');
    WriteLn('         that the signals were intact.');
    WriteLn;
    Write('Enter Test (1-5) or E to end: ');
    repeat
      ch := UpCase(readkey);
    until ch in ['1', '2', '3', '4', '5', 'E', 'Q'];
    WriteLn(ch);
 
    if (Ch = 'E') or (Ch = 'Q') then EndOfTest := True
    else begin
      {Fill buffer depending on choice}
      if ch = '1' then begin
        for i := 0 to 16383 do begin
          Wr^[i] := $80;
          inc(i);
          Wr^[i] := 0;
        end; {for}
      end else if ch = '2' then begin
        for i := 0 to 16383 do begin
          Wr^[i] := $F7FF;
          inc(i);
          Wr^[i] := 0;
        end; {for}
      end else if ch = '3' then begin
        for i := 0 to 16383 do
          Wr^[i] := $1000
      end else if ch in ['4','5'] then begin
        for i := 0 to 16383 do begin
          Wr^[i] := PowerPatterns[1];
          inc(i);
          Wr^[i] := PowerPatterns[2];
        end; {for}
      end;{if/else}
 
      {perform the test}
      WriteLn; Write('Will perform WRITE test first, then the READ.  Data read back will ');
      If Ch <> '5' then write('not ');
      WriteLn; WriteLn('be verified.  Press SPACE to move on to read test once current write');
      WriteLn('test has finished, N to skip on immediately, or S to skip it.');
      DisplayStr := 'Test ' + Ch;
 
      TestMode := PatRead + PatWrite + PatWriteContinuous;
      If Ch = '5' then TestMode := TestMode + PatVerify
      else TestMode := TestMode + PatReadContinuous;
      Errors := Errors + PatternTest(Wr,Rd,DisplayStr,TestMode);
    end;{if/else}
  until EndOfTest;
 
  Dispose(Rd); Dispose(Wr);
  If Errors = 0 then Write('No')
  else write(Errors);
  write(' errors were encountered.');
end;{procedure SignalTest}
 
 
 
function ParamSpecified(s : string) : boolean;
{checks all command line arguments for s, returning true if found}
var i : word; found : boolean;
begin
  found := false;
  for i := 1 to ParamCount do
    if Copy(ParamStr(i),1,Length(s)) = s then found := true;
  ParamSpecified := found;
end;{function ParamSpecified}
 
 
function GetParam(s : string) : string;
{returns the value specified by a paramter, e.g. for 'size=8M' it would
 return '8M'}
var i : word; returnstr : string;
begin
  returnstr := '';
  for i := 1 to ParamCount do
    if Copy(ParamStr(i),1,Length(s)) = s then
      {this is the parameter we're looking for}
      ReturnStr := Copy(ParamStr(i),succ(Length(s)),
                        (Length(ParamStr(i))-Length(s)) );
  GetParam := ReturnStr;
end;{function GetParam}
 
 
function StringToValue(s : string) : LongInt;
{returns the value specified in the string as a LongInt, taking account
 of K and M suffixes (Kilobytes and Megabytes)}
var
  IsMega : Boolean;
  IsKilo : Boolean;
  n      : LongInt;
  Cd     : Integer;
 
begin
  IsKilo := False; IsMega := False;
 
  Case UpCase(s[length(s)]) of
    'K' : IsKilo := True;
    'M' : IsMega := True;
  end;{case}
 
  If IsKilo or IsMega then
    BYTE(s[0]) := Pred(BYTE(s[0])); {chop off suffix, if present}
 
  val(s,n,cd);
  if cd = 0 then
  begin
    {converted OK so apply multiplier}
    if IsKilo then n := n SHL 10;
    if IsMega then n := n SHL 20;
  end;
 
  If (n < 65536) or (cd <> 0) then
  begin
    {didn't understand size= input or < 64KB was specified; set to default}
    writeln('Didn''t understand size parameter.  Must be 64K or more.');
    n := TestSize;
  end;
 
  StringToValue := n;
end;{function StringToValue}
 
 
 
 
{=========================================================================}
{Program main block follows.
{=========================================================================}
 
 
VAR
  ReadSpeed,
  WriteSpeed,
  IOPS          :  Real;
  Ch            :  Char;
  Readonly      :  Boolean;
  TestDone      :  Boolean;
 
BEGIN
  WriteLn('DiskTest, by James Pearce & Foone Turing.  Version ', VERSION);
  If ParamSpecified('/h') or ParamSpecified('-h') or
     ParamSpecified('/?') or ParamSpecified('-?') then
  begin
    WriteLn('Disk and interface performance and reliability testing.');
    WriteLn;
    WriteLn('With no command line parameters, the utility will perform a file-system based');
    WriteLn('performance test with a test file size of 4MB and 256 seeks, with file size');
    WriteLn('truncated to available free space if it is less.');
    WriteLn;
    WriteLn('Performance test specific command line options:');
    WriteLn;
    WriteLn('  * maxseeks  - 4096 seeks (default is 256)');
    WriteLn('  * highseeks - 1024 seeks');
    WriteLn('  * lowseeks  - 128 seeks');
    WriteLn('  * minseeks  - 32 seeks (use for floppy drives)');
    WriteLn('  * size=x    - specify the test file size, which will be truncated to');
    WriteLn('                available free space.  To use all free space use ''maxsize'' ');
    WriteLn('                instead.  Value is in bytes, specify K or M as required.');
    WriteLn('                examples: size=4M (default), size=16M, size=300K');
    WriteLn;
    WriteLn('Example: disktest size=8M maxseeks');
    WriteLn;
    WriteLn('Note: XT class hardware with stepper-motor drives will process random IO at');
    WriteLn('      about 5 - 10 IOPS only, hence 256 seeks is enough for measurement on');
    WriteLn('      such systems.');
    WriteLn;
    Write('Press (c) for reliability testing usage, any other key to quit: ');
    Ch := UpCase(ReadKey);
    If Ch = 'C' then
    begin
      {display reliability testing usage notes}
      WriteLn(Ch); WriteLn;
      WriteLn('Reliability Testing Options:');
      WriteLn;
      WriteLn('  * mediatest  - performs pattern testing instead of performance testing,');
      Writeln('                 reporting errors as it runs.  10 tests.');
      WriteLn;
      WriteLn('  * signaltest - performs pattern tests for checking signal quality and');
      WriteLn('                 measuring power consumption.  These operate interactively,');
      WriteLn('                 hence more help with this option is provided when specified.');
      WriteLn;
      WriteLn('Note: size=xM can also be specified for these tests.');
    end;{if Ch}
  end {help screen}
  else
  begin
    WriteLn;
    TestDone := False;
 
    {check if we're running a read-only test on an existing test file}
    if ParamSpecified('readonly') then readonly := true
    else readonly := false;
 
    if ParamSpecified('noprogress') then noprogress := true
    else noprogress := false; {controls whether progress marks are displayed in performance tests}
 
    if Not Readonly then
    begin
      TestDone := True;
      {First truncate the test file to 0 bytes, if it's present}
      Write('Preparing drive...');
      PurgeTestFile;
 
      {check to see if specific test size was specified with size=}
      if ParamSpecified('size=') then
        TestSize := StringToValue( GetParam('size=') );
 
      {Then check disk space on the current drive, and reduce TestSize
       accordingly}
      If (DiskFree(0) < TestSize) or (ParamSpecified('maxsize')) then
      begin
        TestSize := (DiskFree(0) SHR 15) SHL 15;
        {truncate to 32K boundary}
      end;{if DiskFree}
 
      WriteLn;
 
      If ParamSpecified('mediatest') then MediaTest
      else if ParamSpecified('signaltest') then SignalTest
      else TestDone := False;
    end;
 
    If Not TestDone then
    begin
      {Next check for seek command line options}
      If ParamSpecified('maxseeks') then Seeks := 4096;
      If ParamSpecified('highseeks') then Seeks := 1024;
      If ParamSpecified('lowseeks') then Seeks := 128;
      If ParamSpecified('minseeks') then Seeks := 32;
 
      If ReadOnly then
      begin
        {check for the test file and how big it is}
        Write('Read-only test mode; checking for existing test file...');
        TestSize := CheckTestFile;
        if TestSize = 0 then
        begin
          WriteLn(' file not found.');
          Halt(0);
        end else writeLn(' OK');
      end;{if ReadOnly}
 
      {print test summary}
      Write('Configuration: ',(TestSize div 1024),' KB test file, ');
      WriteLn(Seeks,' IOs in random tests.');
 
      WriteLn;
      If Not Readonly then
      begin
        Write('Write Speed         : ');
        WriteSpeed := CreateFile;
        WriteLn(WriteSpeed:3:2,' KB/s');
      end;{if not Readonly}
 
      Write('Read Speed          : ');
      ReadSpeed := ReadFile;
      WriteLn(ReadSpeed:3:2,' KB/s');
 
      If ReadOnly then
      begin
        Write('8K random read      : ');
        IOPS := RandomTest(8192,100);
      end else begin
        Write('8K random, 70% read : ');
        IOPS := RandomTest(8192,70);
      end;{if ReadOnly}
      WriteLn(IOPS:3:1,' IOPS');
 
      Write('Sector random read  : ');
      IOPS := RandomTest(512,100);
      WriteLn(IOPS:3:1,' IOPS');
 
      WriteLn;
      Write('Average access time (includes latency and file system');
      WriteLn(' overhead), is ',(1000/IOPS):2:0,' ms.');
      WriteLn;
    end;{if not TestDone}

    if Not ReadOnly then
    begin
      DeleteTestFile
    end;{if not ReadOnly}

  end;{if/else}
END.{PROGRAM}