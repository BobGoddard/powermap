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

with Record_Types;             use Record_Types;
with Ada.Calendar;             use Ada.Calendar;
with Ada.Calendar.Arithmetic;  use Ada.Calendar.Arithmetic;

package Time_Routines is

   type inc_years    is private;
   type inc_months   is private;
   type inc_days     is private;
   type inc_minutes  is private;

   procedure reset_time        (t :  in out tmrec; rt : Record_Types.graph_period_type);
   procedure reset_time_offset (t :  in out tmrec);
   procedure populate_tm       (t :  in out tmrec);
   procedure populate_subs     (t :  in out tmrec);
   function  Get_Days_In_Month (t :  Record_Types.tmrec)                          return Day_Count;
   function  Get_Days_In_Month (t1 : Record_Types.tmrec; t2 : Record_Types.tmrec) return Day_Count;
   function  Get_Hours_In_Day  (t1 : Record_Types.tmrec; t2 : Record_Types.tmrec) return Hour_Count;
   function  Get_Time_Str      (t1 : Record_Types.tmrec)                          return String;

   function  add               (t :  Record_Types.tmrec; y  : inc_years)            return Record_Types.tmrec;
   function  add               (t :  Record_Types.tmrec; m  : inc_months)           return Record_Types.tmrec;
   function  add               (t :  Record_Types.tmrec; d  : inc_days)             return Record_Types.tmrec;
   function  add               (t :  Record_Types.tmrec; mn : inc_minutes)          return Record_Types.tmrec;

   function to_inc_years       (m :  Integer)                                       return inc_years;
   function to_inc_months      (m :  Integer)                                       return inc_months;
   function to_inc_days        (m :  Integer)                                       return inc_days;
   function to_inc_minutes     (m :  Integer)                                       return inc_minutes;

private
   type inc_years    is new Integer;
   type inc_months   is new Integer;
   type inc_days     is new Integer;
   type inc_minutes  is new Integer;

end Time_Routines;
