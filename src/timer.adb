with Ada.Calendar;
with Ada.Text_IO;

use Ada.Calendar;

package body Timer is
   function start return T is
      result: T;
   begin
      result.clock := Ada.Calendar.Clock;
      return result;
   end start;

   procedure reset(tm: in out T) is
   begin
      tm.clock := Ada.Calendar.Clock;
   end reset;

   function reset(tm: in out T) return Float is
      oldTime: Ada.Calendar.Time;
   begin
      oldTime := tm.clock;
      tm.clock := Ada.Calendar.Clock;
      return Float(tm.clock - oldTime);
   end reset;

   procedure report(tm: in out T) is
      dur: Float;
   begin
      dur := tm.reset;
      Ada.Text_IO.Put_Line("Elapsed: " & dur'Image);
   end report;

end Timer;
