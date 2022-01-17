unit flcHTMLTest;

interface

procedure Test;

implementation

uses
  flcHTMLCharEntity,
  flcHTMLProperties,
  flcHTMLElements,
  flcHTMLLexer,
  flcHTMLParser;

procedure Test;
begin
  flcHTMLCharEntity.Test;
  flcHTMLProperties.Test;
  flcHTMLElements.Test;
  flcHTMLLexer.Test;
  flcHTMLParser.Test;
end;



end.

