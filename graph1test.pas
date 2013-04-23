program graph1(output);
const d = 0.0625;
lim = 32;
c = 6.28318; 
var x,y : real;  i : integer;
begin
for i := 0 to lim do
    begin
        x := d*i;
        y := exp(-x)*sin(c*x)
    end
end.