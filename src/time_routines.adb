--  Copyright (c) 2021 Bob Goddard <git@1.git.bgcomp.co.uk>
--
--  This file is free software: you may copy, redistribute and/or modify it
--  under the terms of the GNU General Public License as published by the
--  Free Software Foundation, either version 2 of the License, or (at your
--  option) any later version.
--
--  This file is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.

with Ada.Text_IO;              use Ada.Text_IO;
with GNAT.Calendar;            use GNAT.Calendar;
with GNAT.Calendar.Time_IO;
with db_routines;
with Logger;

package body Time_Routines is

   function add (t : Record_Types.tmrec; y : inc_years) return Record_Types.tmrec is
      n : Record_Types.tmrec := t;
      p : inc_years := y;
      d : Day_Count;
   begin
      add_years :
      while p /= 0 loop
         n.tm_year := n.tm_year + Standard.Integer (p);
         if p < 0 then
            p := p + 1;
         else
            p := p - 1;
         end if;
      end loop add_years;
      d := Get_Days_In_Month (n);
      if n.tm_mday > Integer (d) then
         n.tm_mday := Integer (d);
      end if;
      populate_tm (n);
      return n;
   end add;

   function add (t : Record_Types.tmrec; m : inc_months) return Record_Types.tmrec is
      n : Record_Types.tmrec := t;
      p : inc_months := m;
      d : Day_Count;
   begin
      add_months :
      while p /= 0 loop
         if n.tm_mon = Ada.Calendar.Month_Number'Last and then p > 0 then
            n.tm_mon := Ada.Calendar.Month_Number'First;
            n := add (n, Time_Routines.to_inc_years (1));
            p := p - 1;
         elsif n.tm_mon = Ada.Calendar.Month_Number'First and then p < 0 then
            n.tm_mon := Ada.Calendar.Month_Number'Last;
            n := add (n, Time_Routines.to_inc_years (-1));
            p := p + 1;
         elsif p < 0 then
            n.tm_mon := n.tm_mon - 1;
            p := p + 1;
         else
            n.tm_mon := n.tm_mon + 1;
            p := p - 1;
         end if;
      end loop add_months;
      d := Get_Days_In_Month (n);
      if n.tm_mday > Integer (d) then
         n.tm_mday := Integer (d);
      end if;
      populate_tm (n);
      return n;
   end add;

   function add (t : Record_Types.tmrec; d : inc_days) return Record_Types.tmrec is
      n :  Record_Types.tmrec := t;
      p :  inc_days           := d;
      dm : Day_Count;
   begin
      add_days :
      while p /= 0 loop
         dm        := Get_Days_In_Month (n);
         if n.tm_mday >= Integer (dm) and then p > 0 then
            n.tm_mday := Ada.Calendar.Day_Number'First;
            n         := add (n, Time_Routines.to_inc_months (1));
            p         := p - 1;
         elsif n.tm_mday = Ada.Calendar.Day_Number'First and then p < 0 then
            n         := add (n, Time_Routines.to_inc_months (-1));
            dm        := Get_Days_In_Month (n);
            n.tm_mday := Integer (dm);
         elsif p < 0 then
            n.tm_mday := n.tm_mday - 1;
            p         := p + 1;
         else
            n.tm_mday := n.tm_mday + 1;
            p         := p - 1;
         end if;
      end loop add_days;
      populate_tm (n);
      return n;
   end add;

   function add (t : Record_Types.tmrec; mn : inc_minutes) return Record_Types.tmrec is
      n :  Record_Types.tmrec;
   begin
      n.tm := t.tm + Duration (mn) * 60.0;
      populate_subs (n);
      return n;
   end add;

   function  Get_Days_In_Month (t : Record_Types.tmrec) return Day_Count is
      d : Day_Count;
   begin
      if t.tm_mon = 1 or else t.tm_mon = 3 or else t.tm_mon = 5 or else t.tm_mon = 7 or else t.tm_mon = 8 or else t.tm_mon = 10 or else t.tm_mon = 12 then
         d := 31;
      elsif t.tm_mon = 4 or else t.tm_mon = 6 or else t.tm_mon = 9 or else t.tm_mon = 11 then
         d := 30;
      elsif t.tm_mon = 2 then
         if t.tm_year rem 4 = 0 and then t.tm_year rem 400 /= 0 then
            d := 29;
         else
            d := 28;
         end if;
      end if;
      return d;
   end Get_Days_In_Month;

   function  Get_Days_In_Month (t1 : Record_Types.tmrec; t2 : Record_Types.tmrec) return Day_Count is
      d  : Day_Count := 0;
      t3 : Record_Types.tmrec := t1;
      t4 : Record_Types.tmrec := t2;
   begin
      count_days :
      loop
         exit count_days when t3.tm_year = t4.tm_year and then t3.tm_mon = t4.tm_mon and then t3.tm_mday = t4.tm_mday;
         d := d + 1;
         if t3.tm < t4.tm then
            t3 := add (t3, d => 1);
         else
            t4 := add (t4, d => 1);
         end if;
      end loop count_days;
      return d;
   end Get_Days_In_Month;

   function  Get_Hours_In_Day  (t1 : Record_Types.tmrec; t2 : Record_Types.tmrec) return Hour_Count is
      h  : Hour_Count := 0;
      t3 : Record_Types.tmrec := t1;
      t4 : Record_Types.tmrec := t2;
   begin
      count_hours :
      loop
         exit count_hours when t3.tm_year = t4.tm_year and then t3.tm_mon = t4.tm_mon and then t3.tm_mday = t4.tm_mday and then t3.tm_hour = t4.tm_hour;
         h := h + 1;
         if t3.tm < t4.tm then
            t3 := add (t3, mn => 60);
         else
            t4 := add (t4, mn => 60);
         end if;
      end loop count_hours;
      return h;
   end Get_Hours_In_Day;

   function Get_Time_Str (t1 : Record_Types.tmrec) return String is
   begin
      return GNAT.Calendar.Time_IO.Image (t1.tm, "%c");
   end Get_Time_Str;

   procedure populate_subs (t : in out tmrec) is
   begin
      GNAT.Calendar.Split_At_Locale (t.tm, t.tm_year, t.tm_mon, t.tm_mday, t.tm_hour, t.tm_minute, t.tm_seconds, t.tm_duration);
   end populate_subs;

   procedure populate_tm (t : in out tmrec) is
   begin
      t.tm := GNAT.Calendar.Time_Of_At_Locale (t.tm_year, t.tm_mon, t.tm_mday, t.tm_hour, t.tm_minute, t.tm_seconds, t.tm_duration);
      t.tm_wday := GNAT.Calendar.Day_Of_Week (t.tm);
   end populate_tm;

   procedure reset_time (t : in out tmrec; rt : graph_period_type) is
   begin
      populate_subs (t);
      t.tm_hour     := Hour_Number'First;
      t.tm_minute   := Minute_Number'First;
      t.tm_seconds  := Second_Number'First;
      t.tm_duration := Second_Duration'First;

      if rt = Record_Types.year then
         t.tm_mon      := Month_Number'First;
         t.tm_mday     := Day_Number'First;
      elsif rt = Record_Types.month then
         t.tm_mday     := Day_Number'First;
      elsif rt = Record_Types.week then
         populate_tm (t);
         decrease_wday :
         loop
            exit decrease_wday when Day_Of_Week (t.tm) = Saturday;
            t := add (t, to_inc_days (-1));
         end loop decrease_wday;
      end if;

      populate_tm (t);
   end reset_time;

   procedure reset_time_offset (t : in out tmrec) is
      new_t : tmrec;
   begin
      populate_subs (t);
      t.tm_hour     := Hour_Number'First;
      t.tm_minute   := Minute_Number'First;
      t.tm_seconds  := Second_Number'First;
      t.tm_duration := Second_Duration'First;
      t.tm_mday     := Day_Number'First;
      populate_tm (t);
      new_t := t;
      db_routines.bill_date (new_t);

      if new_t.tm > t.tm then
         t := add (t, to_inc_months (-1));
      end if;

      populate_tm (t);
   end reset_time_offset;

   function to_inc_days (m : Integer) return inc_days is
   begin
      return inc_days (m);
   end to_inc_days;

   function to_inc_minutes (m : Integer) return inc_minutes is
   begin
      return inc_minutes (m);
   end to_inc_minutes;

   function to_inc_months (m : Integer) return inc_months is
   begin
      return inc_months (m);
   end to_inc_months;

   function to_inc_years (m : Integer) return inc_years is
   begin
      return inc_years (m);
   end to_inc_years;

end Time_Routines;
