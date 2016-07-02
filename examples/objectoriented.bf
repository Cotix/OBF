%

This is a very simple example that shows off our object and overloading system.
In this example there are two objects defined. A Rectangle, and a Group, which
is a shape based on other shapes(including groups).

Every shape has a function surface, which calculates it's surface.

Every shape also has an function insert. It inserts itself into a group.

Notice how the insert of a Rectangle looks complicated, while the insert of a
group is very elegant and simple. It just calls insert on every member of that.


Group also has a function a, which adds a shape to itself. It just calls insert
on the other object, without caring what that object is, since i is implemented
in both cases.

This example calculates the surface of two rectangle that are 6*6 and 4*6.
The surface of the group is thus 6*6+4*6 = 56. This corresponds to the ascii
character '8'. It should thus print the number '8'.
%

:R:^*b?>b&:
:Ri:$*[>]?>b<R*$*$a>$>$a&&$&:
:Rs:$^b+$*$m$>$m$&:
:G:^*b&:
:Gi:*[i>]&:
:Ga:$i$:
:Gs:?>b<*[s?>b&>a<*[>]!>]&>$^ba$!:

R*++++++>++++++&?>                %6*6 rectangle%
R*++++>+++++&?>                   %4*5 rectangle%
G@<<$a$>$a                        %add both rectangle%
?>b@<s>.                          %Calculates surface%
