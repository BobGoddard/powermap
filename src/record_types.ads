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

with Ada.Calendar;            use Ada.Calendar;
with Ada.Calendar.Arithmetic;
with Ada.Calendar.Formatting;
with GNAT.Calendar;

package Record_Types is

   type graph_period_type is (year, month, offset, day, week, blind);
   type env_type is (humidity, lux, pressure, temperature);
   type power_type is (power);

   type Hour_Count is range
     -(24 * (366 * (1 + Year_Number'Last - Year_Number'First)))
     ..
     +(24 * (366 * (1 + Year_Number'Last - Year_Number'First)));

   type tmrec is record
      tm          :  Ada.Calendar.Time := Ada.Calendar.Formatting.Time_Of
                    (Ada.Calendar.Year_Number'First,
                     Ada.Calendar.Month_Number'First,
                     Ada.Calendar.Day_Number'First,
                     Time_Zone => 0);
      tm_year     : Ada.Calendar.Year_Number;
      tm_mon      : Ada.Calendar.Month_Number;
      tm_mday     : Ada.Calendar.Day_Number;
      tm_wday     : GNAT.Calendar.Day_Name;
      tm_hour     : GNAT.Calendar.Hour_Number;
      tm_minute   : GNAT.Calendar.Minute_Number;
      tm_seconds  : GNAT.Calendar.Second_Number;
      tm_duration : GNAT.Calendar.Second_Duration;
   end record;

   type periodrec is record
      periodstart : tmrec;
      periodend   : tmrec;
   end record;

   type avgrec is record
      tm                : Ada.Calendar.Time;
      hr1_avg           : Standard.Float;
      hr2_avg           : Standard.Float;
      hr4_avg           : Standard.Float;
      hr6_avg           : Standard.Float;
      hr12_avg          : Standard.Float;
      hr24_avg          : Standard.Float;
      days_7_avg        : Standard.Float;
      days_31_avg       : Standard.Float;
      current_week_avg  : Standard.Float;
      current_month_avg : Standard.Float;
   end record;

   type Min_Max is record
      min            : Long_Integer;
      max            : Long_Integer;
   end record;

   type powerrec is record
      period_details   : periodrec;
      day_count        : Ada.Calendar.Arithmetic.Day_Count := 0;
      power_used       : Long_Integer := 0;
      kwcostnet        : Long_Integer := 0;
      sccostnet        : Long_Integer := 0; -- sc
      kwcostgross      : Long_Integer := 0; --    * VAT
      sccostgross      : Long_Integer := 0; -- sc * VAT

      totalpower       : Long_Integer := 0;
      totalkwcostnet   : Long_Integer := 0;
      totalkwcostgross : Long_Integer := 0;
      totalsccostnet   : Long_Integer := 0;
      totalsccostgross : Long_Integer := 0;
   end record;

   type pow_array is array (Positive range <>) of powerrec;

   type bill_date_details is record
      s : Ada.Calendar.Time;
      d : Ada.Calendar.Day_Number;
   end record;

   type graph_divs is record
      Major, NoDivs, YDist : Integer;
      Max                         : Float;
   end record;

   type graph_divs_array is array (Positive range <>) of graph_divs;

   --  Max / NoDivs = line number
   --  YDist / NoDivs = distance between lines
   gd : constant graph_divs_array :=
          ((1,     1, 240,     0.1),
           (5,    10, 240,     0.3),
           (4,    10, 240,     0.4),
           (5,    10, 240,     0.5),
           (5,    14, 240,     0.7),
           (5,    16, 240,     0.8),
           (5,     9, 240,     0.9),
           (5,    10, 240,     1.0),
           (5,    10, 240,     1.2),
           (5,    10, 240,     1.3),
           (5,    10, 240,     1.4),
           (5,    10, 240,     1.5),
           (5,    10, 240,     1.6),
           (5,    10, 240,     1.7),
           (5,    10, 240,     1.8),
           (5,    10, 240,     1.9),
           (5,    10, 240,     2.0),
           (5,    10, 240,     2.5),
           (5,    10, 240,     3.0),
           (5,     6, 240,     6.0),
           (5,     7, 240,     7.0),
           (5,     8, 240,     8.0),
           (5,    16, 240,   160.0),
           (5,    17, 240,   170.0),
           (5,    18, 240,   180.0),
           (5,    20, 240,   200.0),
           (100,  20, 240,   400.0),
           (500, 200, 240, 20000.0)
          );

   subtype HRS_3 is String (1 .. 3);
   HR3_Whistle :  constant array (Integer range 1 .. 9) of HRS_3 :=
                   ("1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th");

   subtype HRS_4 is String (1 .. 4);
   HR4_Whistle :  constant array (Integer range 10 .. 31) of HRS_4 :=
                    ("10th", "11th", "12th", "13th", "14th", "15th",
                     "16th", "17th", "18th", "19th", "20th", "21st",
                     "22nd", "23rd", "24th", "25th", "26th", "27th",
                     "28th", "29th", "30th", "31st");

   subtype S3 is String (1 .. 3);
   Month_Names :  constant array (Ada.Calendar.Month_Number) of S3 :=
                   ("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec");

   type use_offset is (left, centre, right);

end Record_Types;
