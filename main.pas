{*
 * Tyler Filla
 * February 4, 2019
 * CS 4500-001 :: Intro to Software Profession
 *
 * TODO: Add high-level theory of operation
 *
 *}

{$I+}
{$mode objfpc}

program main;

uses sysutils;

const
  { The name of the input file. }
  C_FILENAME_IN = 'HW1infile.txt';

  { The name of the output file. }
  C_FILENAME_OUT = 'HW1fillaOutfile.txt';

type
  {*
   * An exception for input file errors.
   *
   * This is a catch-all exception thrown by the input file parser procedure
   * when something doesn't go its way. Its message contains more details.
   *}
  EInputFileException = class(Exception);

  { Pointer to TCircle. See below. }
  TCirclePtr = ^TCircle;

  {*
   * A circle on the whiteboard.
   *
   * One of these records tracks the relevant state of exactly one circle drawn
   * on the imaginary whiteboard. Each circle knows its number (thus obviating
   * the need to iterate the global circle array), and each circle counts its
   * virtual checkmarks.
   *
   * The in-memory graph construction is based on simple pointers.
   *}
  TCircle = record
    { The number assigned to this circle. }
    Number: integer;

    { The number of checkmarks placed on the circle. }
    Marks: integer;

    { Connections to other circles (the arrows originating here). }
    Arrows: array of TCirclePtr;
  end;

var
  { The output text file. }
  OutputFile: TextFile;

  { Tracker for all allocated circles. }
  AllCircles: array of TCircle;

  { A pointer to the current circle. }
  CurrentCircle: TCirclePtr;

{*
 * Write output to stdout and the output file.
 *
 * @param Text The output text
 *}
procedure MyWriteLn(const Text: string);
begin
  { To screen. }
  WriteLn(Output, Text);

  { To file. }
  WriteLn(OutputFile, Text);
end;

{*
 * Set up the circles from the given input file.
 *
 * @param Name The input file name
 *}
procedure InitCirclesFromFile(const Name: string);
var
  { The opened input file. }
  InputFile: TextFile;

  { The number N. The number of circles. }
  N: integer;

  { The number K. The number of arrows. }
  K: integer;

  { An iterator index over 1..K. }
  i: integer;

  { A temporary number of an arrow. }
  ArrowNum: integer;

  { A temporary index of an arrow's source circle. }
  ArrowSrc: integer;

  { A temporary index of an arrow's destination circle. }
  ArrowDest: integer;
begin
  { The input file. }
  AssignFile(InputFile, Name);

  try
    try
      { Open the input file for read. }
      Reset(InputFile);

      { Try to read variable N (number of circles). }
      if Eof(InputFile) then
        raise EInputFileException.create('Failed to read N: Premature EOF');
      try
        ReadLn(InputFile, N);
      except
        on E: Exception do
          raise EInputFileException.create(Format('Failed to read N: %s: %s', [E.ClassName, E.Message]));
      end;

      { Check range of N (from 2 to 10). }
      if (N < 2) or (N > 10) then
        raise EInputFileException.create(Format('N is out of range: %d', [N]));

      { Allocate N circles. }
      SetLength(AllCircles, N);

      { Initialize allocated circles. }
      for i := 1 to N do
        begin
          AllCircles[i - 1].Number := i;
          AllCircles[i - 1].Marks := 0;
          AllCircles[i - 1].Arrows := nil;
        end;

      { Try to read variable K (number of arrows). }
      if Eof(InputFile) then
        raise EInputFileException.create('Failed to read K: Premature EOF');
      try
        ReadLn(InputFile, K);
      except
        on E: Exception do
          raise EInputFileException.create(Format('Failed to read K: %s: %s', [E.ClassName, E.Message]));
      end;

      { Check range of K (from 0 to inf). }
      if (K < 0) then
        raise EInputFileException.create(Format('K is out of range: %d', [K]));

      { Read in K arrow definitions. }
      for ArrowNum := 1 to K do
        begin
          { Try to read arrow source (circle index). }
          if Eof(InputFile) then
            raise EInputFileException.create(Format('Failed to read source for arrow %d: Premature EOF', [ArrowNum]));
          try
            Read(InputFile, ArrowSrc);
          except
            on E: Exception do
              raise EInputFileException.create(Format('Failed to read source for arrow %d: %s: %s', [ArrowNum, E.ClassName, E.Message]));
          end;

          { Check range of arrow source index (from 1 to N). }
          if (ArrowSrc < 1) or (ArrowSrc > N) then
            raise EInputFileException.create(Format('Source index for arrow %d is out of range: %d', [ArrowNum, ArrowSrc]));

          { Try to read arrow destination (circle index). }
          if Eof(InputFile) then
            raise EInputFileException.create(Format('Failed to read destination for arrow %d: Premature EOF', [ArrowNum]));
          try
            Read(InputFile, ArrowDest);
          except
            on E: Exception do
              raise EInputFileException.create(Format('Failed to read destination for arrow %d: %s: %s', [ArrowNum, E.ClassName, E.Message]));
          end;

          { Check range of arrow destination index (from 1 to N). }
          if (ArrowDest < 1) or (ArrowDest > N) then
            raise EInputFileException.create(Format('Destination index for arrow %d is out of range: %d', [ArrowNum, ArrowDest]));

          { Establish the arrow connection in memory. This increments the length
            of the source circle's arrow array and appends to it a pointer to
            the destination circle. }
          SetLength(AllCircles[ArrowSrc - 1].Arrows, Length(AllCircles[ArrowSrc - 1].Arrows) + 1);
          AllCircles[ArrowSrc - 1].Arrows[High(AllCircles[ArrowSrc - 1].Arrows)] := @AllCircles[ArrowDest - 1];
        end;

      { Set the first circle as current. }
      CurrentCircle := @AllCircles[0];
    except
      { Rethrow miscellaneous I/O errors under catch-all exception. }
      on E: EInOutError do
        raise EInputFileException.create(Format('Failed to read input file: %s: %s ', [E.ClassName, E.Message]));
    end;
  finally
    { Close the input file. }
    CloseFile(InputFile);
  end;
end;

{*
 * Mark the current circle.
 *}
procedure MarkCurrentCircle;
begin
  { Increment the mark count on the current circle. }
  Inc(CurrentCircle^.Marks);
end;

{ Program entry point. }
begin
  { The output file. This will be used to produce a transcript of the game. }
  AssignFile(OutputFile, C_FILENAME_OUT);

  { Try to open the output file for write (creating it if needed). }
  try
    Rewrite(OutputFile);
  except
    on E: EInOutError do
      begin
        WriteLn(Format('Failed to open output file for write: %s', [E.Message]));
        Exit;
      end;
  end;

  { Try to initialize from the input file. }
  try
    InitCirclesFromFile(C_FILENAME_IN);
  except
    on E: EInputFileException do
      begin
        MyWriteLn(Format('Input file error: %s', [E.Message]));
        CloseFile(OutputFile);
        Exit;
      end;
  end;

  { Mark the first circle as current. }
  MarkCurrentCircle();
end.
