with Ada.Calendar;

package Timer is
   type T is tagged private;

   function start return T;
   function reset(tm: in out T) return Float;
   procedure reset(tm: in out T);
   procedure report(tm: in out T);

private
   type T is tagged record
      clock: Ada.Calendar.Time;
   end record;
end Timer;
