{*
 * Tyler Filla
 * February 4, 2019
 * CS 4500-001 :: Intro to Software Profession
 *
 * TODO: Add high-level theory of operation
 *
 *}

program main;

type
  { A circle on the whiteboard. }
  TCircle = record
    { The number assigned to this circle. }
    Number: integer;

    { The number of checkmarks placed on the circle. }
    Marks: integer;

    { Connections to other circles (the arrows originating here). }
    Arrows: array of ^TCircle;
  end;

var
  { All loaded circles. }
  AllCircles: array of TCircle;

  { A pointer to the current circle. }
  CircleCurrent: ^TCircle;

{*
 * Mark the current circle.
 *}
procedure MarkCurrentCircle();
begin
  { Increment the mark count on the current circle. }
  Inc(CircleCurrent^.Marks);
end;

{*
 * Program entry point.
 *}
begin
  { Mark the initial current circle. }
  MarkCurrentCircle();
end.
