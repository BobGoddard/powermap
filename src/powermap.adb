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

with Ada.Calendar;             use Ada.Calendar;
with Ada.Calendar.Conversions;
with Ada.Directories;          use Ada.Directories;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
with GNAT.Calendar;
with GNAT.Command_Line;
with Interfaces.C;
with db_routines;              use db_routines;
with Time_Routines;            use Time_Routines;
with Record_Types;             use Record_Types;
with Print_Line;               use Print_Line;
with Update_Record;            use Update_Record;
with Construct_SVG;            use Construct_SVG;
with Generate_HTML;
with Config_Handler;

function Powermap return Integer is
   index            : Integer;
   epoch            : tmrec;
   epoch_year       : tmrec;
   epoch_month      : tmrec;
   epoch_offset     : tmrec;
   epoch_week       : tmrec;
   epoch_day        : tmrec;
   ts_start         : tmrec;
   ctime            : tmrec;
   gtime_start      : tmrec;
   gtime_end        : tmrec;
   stime_start      : tmrec;
   stime_end        : tmrec;
   stime_start_off  : tmrec;
   stime_end_off    : tmrec;
   earliest_start   : tmrec;
   period_details   : periodrec;
   half_hour_Count  : Hour_Count;
   is_year          : Boolean := False;
   is_month         : Boolean := False;
   is_offset        : Boolean := False;
   is_week          : Boolean := False;
   is_day           : Boolean := False;
   tmp_bool         : Boolean := False;
   tmp_ts           : Ada.Calendar.Time := GNAT.Calendar.No_Time;
   XML_Settings     : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String ("/etc/powermap.xml");
   CMD_Args         : exception;
   pragma Warnings (Off, tmp_bool);
begin
   loop
      case GNAT.Command_Line.Getopt ("y m o w d t: i:") is
         when 'y' =>
            is_year := True;
         when 'm' =>
            is_month := True;
         when 'o' =>
            is_offset := True;
         when 'w' =>
            is_week := True;
         when 'd' =>
            is_day := True;
         when 't' =>
            tmp_ts := Ada.Calendar.Conversions.To_Ada_Time (Interfaces.C.long'Value (GNAT.Command_Line.Parameter));
         when 'i' =>
            XML_Settings := Ada.Strings.Unbounded.To_Unbounded_String (GNAT.Command_Line.Parameter);
         when others =>
            exit;
      end case;
   end loop;

   if not Ada.Directories.Exists (Ada.Strings.Unbounded.To_String (XML_Settings)) then
      raise CMD_Args with "Problem reading " & Ada.Strings.Unbounded.To_String (XML_Settings);
   end if;

   if Ada.Directories.Kind (Ada.Strings.Unbounded.To_String (XML_Settings)) /= Ada.Directories.Ordinary_File then
      raise CMD_Args with "Config file does not appear to be an ordinary file";
   end if;

   Config_Handler.Set_Config_Name (XML_Settings);
   Config_Handler.Load_Config;
   ctime.tm := Ada.Calendar.Clock;
   populate_subs (ctime);
   db_connect;
   earliest_start.tm := Ada.Calendar.Conversions.To_Ada_Time (db_routines.get_earliest_start);
   ts_start := ctime;

   if tmp_ts /= GNAT.Calendar.No_Time then
      if tmp_ts > ctime.tm then
         ts_start.tm := ctime.tm;
      end if;
      if tmp_ts < earliest_start.tm then
         ts_start := earliest_start;
      end if;
   end if;
--   return -1;

   if not is_year and not is_month and not is_offset and not is_week and not is_day then
      is_year   := True;
      is_month  := True;
      is_offset := True;
      is_week   := True;
      is_day    := True;
   end if;

   epoch := ts_start;
   Time_Routines.populate_subs (epoch);
   Time_Routines.populate_subs (ctime);
   epoch_year   := epoch;
   epoch_month  := epoch;
   epoch_offset := epoch;
   epoch_week   := epoch;
   epoch_day    := epoch;

   if is_year then
      declare
         power_details     : pow_array (1 ..  12);
         power_details_off : pow_array (1 ..  12);
      begin
         Time_Routines.reset_time (epoch_year, Record_Types.year);
         gtime_start       := epoch_year;
         gtime_end         := Time_Routines.add (gtime_start, Time_Routines.to_inc_years (1));

         year_yearly :
         while gtime_start.tm_year <= ctime.tm_year loop
            stime_start      := gtime_start;
            stime_end        := Time_Routines.add (stime_start, Time_Routines.to_inc_months (1));
            stime_start_off  := stime_start;
            stime_end_off    := stime_end;
            bill_date (stime_start_off);
            bill_date (stime_end_off);
            index := 1;

            year_monthly :
            loop
               Print_Line_Year (stime_start, stime_end, stime_start_off, stime_end_off);
               period_details.periodstart := stime_start;
               period_details.periodend   := stime_end;
               power_details (index) := get_power_usage (period_details, Record_Types.year, ctime);

               if index = 1 then
                    Update_Record_r (r => power_details (index));
               else
                    Update_Record_l_r (l => power_details (index - 1), r => power_details (index));
               end if;

               period_details.periodstart         := stime_start_off;
               period_details.periodend           := stime_end_off;
               power_details_off (index) := get_power_usage (period_details, Record_Types.year, ctime);
               power_details_off (index).period_details := period_details;

               if index = 1 then
                    Update_Record_r (r => power_details_off (index));
               else
                    Update_Record_l_r (l => power_details_off (index - 1), r => power_details_off (index));
               end if;

               exit year_monthly when stime_end.tm = gtime_end.tm;

               stime_start_off := stime_end_off;
               stime_start     := stime_end;
               stime_end     := Time_Routines.add (stime_end,     Time_Routines.to_inc_months (1));
               stime_end_off := Time_Routines.add (stime_end_off, Time_Routines.to_inc_months (1));
               bill_date (stime_end_off);
               index := index + 1;
            end loop year_monthly;

            Write_SVG (power_details, power_details_off);
            if tmp_bool then
               return (4);
            end if;
            gtime_start := gtime_end;
            gtime_end   := Time_Routines.add (gtime_end, Time_Routines.to_inc_years (1));
            Print_Power_Array (power_details);
            Print_Power_Array (power_details_off);
         end loop year_yearly;
      end;
   end if;

   if is_month then
      Time_Routines.reset_time (epoch_month, Record_Types.month);
      gtime_start       := epoch_month;
      gtime_end := Time_Routines.add (gtime_start, Time_Routines.to_inc_months (1));

      month_monthly :
      while gtime_start.tm <= ctime.tm loop
         stime_start := gtime_start;
         stime_end   := Time_Routines.add (stime_start, Time_Routines.to_inc_days (1));
         index := 1;

         declare
            power_details : pow_array (1 .. Integer (Get_Days_In_Month (gtime_start, gtime_end)));
         begin

            month_daily :
            loop
               Print_Line_Month (stime_start, stime_end);
               period_details.periodstart := stime_start;
               period_details.periodend   := stime_end;
               power_details (index)       := get_power_usage (period_details, Record_Types.month, ctime);
               power_details (index).period_details := period_details;

               if index = 1 then
                    Update_Record_r (power_details (index));
               else
                    Update_Record_l_r (power_details (index - 1), power_details (index));
               end if;

               exit month_daily when stime_end.tm = gtime_end.tm;
               stime_start := stime_end;
               stime_end   := Time_Routines.add (stime_start, Time_Routines.to_inc_days (1));
               index := index + 1;
            end loop month_daily;

            Write_SVG (power_details, Record_Types.month);
            gtime_start := gtime_end;
            gtime_end   := Time_Routines.add (gtime_end, Time_Routines.to_inc_months (1));
            Print_Power_Array (power_details);
         end;
      end loop month_monthly;
   end if;

   if is_offset then
      Time_Routines.reset_time_offset (epoch_offset);
      gtime_start       := epoch_offset;
      gtime_end := Time_Routines.add (gtime_start, Time_Routines.to_inc_months (1));
      bill_date (gtime_start);
      bill_date (gtime_end);

      offset_monthly :
      while gtime_start.tm <= ctime.tm loop
         stime_start := gtime_start;
         stime_end   := Time_Routines.add (stime_start, Time_Routines.to_inc_days (1));
         index := 1;

         declare
            power_details : pow_array (1 .. Integer (Get_Days_In_Month (gtime_start, gtime_end)));
         begin
            offset_daily :
            loop
               Print_Line_Month (stime_start, stime_end);
               period_details.periodstart := stime_start;
               period_details.periodend   := stime_end;
               power_details (index)       := get_power_usage (period_details, Record_Types.offset, ctime);
               power_details (index).period_details := period_details;

               if index = 1 then
                    Update_Record_r (power_details (index));
               else
                    Update_Record_l_r (power_details (index - 1), power_details (index));
               end if;

               exit offset_daily when stime_end.tm = gtime_end.tm;
               stime_start := stime_end;
               stime_end   := Time_Routines.add (stime_start, Time_Routines.to_inc_days (1));
               index := index + 1;
            end loop offset_daily;

            Write_SVG (power_details, Record_Types.offset);
            gtime_start := gtime_end;
            gtime_end   := Time_Routines.add (gtime_end, Time_Routines.to_inc_months (1));
            bill_date (gtime_end);
            Print_Power_Array (power_details);
         end;
      end loop offset_monthly;
   end if;

   if is_week then
      Time_Routines.reset_time (epoch_week, Record_Types.week);
      gtime_start       := epoch_week;
      gtime_end := Time_Routines.add (gtime_start, Time_Routines.to_inc_days (7));

      daily :
      while gtime_start.tm <= ctime.tm loop
         stime_start := gtime_start;
         stime_end   := Time_Routines.add (stime_start, Time_Routines.to_inc_days (1));
         index := 1;
         Print_Line_Month (gtime_start, gtime_end);

         declare
            power_details : pow_array (1 .. 7);
         begin

            day_daily :
            loop
               Print_Line_Month (stime_start, stime_end);
               period_details.periodstart := stime_start;
               period_details.periodend   := stime_end;
               power_details (index)       := get_power_usage (period_details, Record_Types.week, ctime);
               power_details (index).period_details := period_details;

               if index = 1 then
                    Update_Record_r (power_details (index));
               else
                    Update_Record_l_r (power_details (index - 1), power_details (index));
               end if;
               --  do work

               exit day_daily when stime_end.tm = gtime_end.tm;
               stime_start := stime_end;
               stime_end   := Time_Routines.add (stime_start, Time_Routines.to_inc_days (1));
               index := index + 1;
            end loop day_daily;

            Write_SVG (power_details, Record_Types.week);
            gtime_start := gtime_end;
            gtime_end   := Time_Routines.add (gtime_end, Time_Routines.to_inc_days (7));
            Print_Power_Array (power_details);
         end;
      end loop daily;
   end if;

   if is_day then
      Time_Routines.reset_time (epoch_day, Record_Types.day);
      gtime_start       := epoch_day;
      gtime_end := Time_Routines.add (gtime_start, Time_Routines.to_inc_days (1));

      m_daily :
      while gtime_start.tm <= ctime.tm loop
         stime_start := gtime_start;
         stime_end   := Time_Routines.add (stime_start, Time_Routines.to_inc_minutes (30));
         index := 1;
         half_hour_Count := Get_Hours_In_Day (gtime_start, gtime_end) * 2;

         declare
            power_details : pow_array (1 .. Integer (half_hour_Count));
         begin

            minutely :
            loop
               Print_Line_Month (stime_start, stime_end);
               period_details.periodstart := stime_start;
               period_details.periodend   := stime_end;
               power_details (index)       := get_power_usage (period_details, Record_Types.day, ctime);
               power_details (index).period_details := period_details;

               if index = 1 then
                    Update_Day_Record_r (power_details (index));
               else
                    Update_Day_Record_l_r (power_details (index - 1), power_details (index));
               end if;

               exit minutely when stime_end.tm = gtime_end.tm;
               stime_start := stime_end;
               stime_end   := Time_Routines.add (stime_start, Time_Routines.to_inc_minutes (30));
               index := index + 1;

            end loop minutely;

            Write_SVG (power_details, Record_Types.day);
            gtime_start := gtime_end;
            gtime_end   := Time_Routines.add (gtime_end, Time_Routines.to_inc_days (1));
            Print_Power_Array (power_details);
         end;
      end loop m_daily;
   end if;

   db_disconnect;
   Generate_HTML.update;
   return (0);
end Powermap;
