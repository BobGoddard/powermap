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

with Ada.Calendar;                 use Ada.Calendar;
with Ada.Calendar.Conversions;
with Ada.Calendar.Arithmetic;      use Ada.Calendar.Arithmetic;
with Time_Routines;                use Time_Routines;
with AdaBase.Results.Sets;

package body db_routines is

   row           : AdaBase.Results.Sets.Datarow;
   numrows       : AdaBase.Affected_Rows;
   costs_row     : AdaBase.Results.Sets.Datarow;
   costs_numrows : AdaBase.Affected_Rows;

   procedure Bill_Date (t : in out Record_Types.tmrec) is
      d : bill_date_details;
   begin
      d := get_bill_date (t);

      if t.tm < d.s then
         t := add (t, Time_Routines.to_inc_months (1));
      end if;

      t.tm_mday  := d.d;
      populate_tm (t);
   end Bill_Date;

   procedure DB_Connect is
   begin
      DR.basic_connect (database => Ada.Strings.Unbounded.To_String (Database),
                        username => Ada.Strings.Unbounded.To_String (DB_User),
                        password => Ada.Strings.Unbounded.To_String (DB_Pass),
                        hostname => Ada.Strings.Unbounded.To_String (DB_Host),
                        port     => DB_Port);
   end DB_Connect;

   procedure DB_Disconnect is
   begin
      DR.disconnect;
   end DB_Disconnect;

   function Get_Bill_Date (t : tmrec) return bill_date_details is
      sql_billdate : constant String := "SELECT ts_start,day_of_month FROM bill_date where ts_start<=" & Ada.Calendar.Conversions.To_Unix_Time (t.tm)'Img &
        " AND ts_end>="   & Ada.Calendar.Conversions.To_Unix_Time (t.tm)'Img;
      dbt : bill_date_details;
   begin
      declare
         stmt : Stmt_Type_Local := DR.query (sql_billdate);
      begin
         numrows := stmt.rows_returned;
         row := stmt.fetch_next;
      end;

      dbt.s := Ada.Calendar.Conversions.To_Ada_Time (Interfaces.C.long (row.column (1).as_byte8));
      dbt.d := Ada.Calendar.Day_Number                                 (row.column (2).as_byte4);
      return dbt;
   end Get_Bill_Date;

   function Get_Earliest_Start return long is
      sql_minsecs : constant String := "SELECT MIN(seconds) FROM count";
   begin
      declare
         stmt : Stmt_Type_Local := DR.query (sql_minsecs);
      begin
         numrows := stmt.rows_returned;
         row := stmt.fetch_next;
      end;

      return (long (row.column (1).as_byte8));
   end Get_Earliest_Start;

   function Get_Last_TS return Interfaces.C.long is
      SQL_Max_Secs          : constant String := "SELECT MAX(seconds) FROM count";
   begin
      declare
         stmt : Stmt_Type_Local := DR.query (SQL_Max_Secs);
      begin
         numrows := stmt.rows_returned;
         row := stmt.fetch_next;
      end;

      return (Interfaces.C.long (row.column (1).as_byte8));
   end Get_Last_TS;

   function Get_Last_TS_Run return Interfaces.C.long is
      SQL_Max_Secs          : constant String := "SELECT MAX(seconds) FROM count";
      SQL_Last_Run          : constant String := "SELECT value FROM config WHERE parameter='latest_ts'";
      SQL_Insert_Secs_Start : constant String := "UPDATE config SET value=";
      SQL_Insert_Secs_End   : constant String := " where parameter = 'latest_ts'";
      Last_Run              : Interfaces.C.long;
      Max_Run               : Interfaces.C.long;
   begin
      declare
         stmt : Stmt_Type_Local := DR.query (SQL_Max_Secs);
      begin
         numrows := stmt.rows_returned;
         row     := stmt.fetch_next;
         Max_Run := Interfaces.C.long (row.column (1).as_byte8);
      end;

      declare
         stmt : Stmt_Type_Local := DR.query (SQL_Last_Run);
      begin
         numrows  := stmt.rows_returned;
         row      := stmt.fetch_next;
         Last_Run := Interfaces.C.long (row.column (1).as_byte8);
      end;

      if DR.execute (sql => SQL_Insert_Secs_Start & Max_Run'Image & SQL_Insert_Secs_End) /= 1 then
         DR.rollback;
      end if;

      DR.commit;

      return (Last_Run);
   end Get_Last_TS_Run;

   function Get_Power_Usage (p : periodrec; pt : graph_period_type; ctime : tmrec) return powerrec is
      kwhr                 : Long_Integer := 0;
      usage                : powerrec;
      localperiodrec       : periodrec := p;
      days_billed          : Day_Count := 0;
      total_days_billed    : Day_Count := 0;
      standing_charge_rate : Long_Integer := 0;
      kwhr_rate            : Long_Integer := 0;
      VAT_rate             : Long_Integer := 0;
      kwcostnet            : Long_Integer := 0;
      sccostnet            : Long_Integer := 0;
      kwcostgross          : Long_Integer := 0;
      sccostgross          : Long_Integer := 0;
      sql2                 : constant String    := "SELECT ts_start, time_string, ts_end, standing_charge_rate, kwhr_rate, VAT_rate from costs_beta where ts_end > "    &
                                                              Ada.Calendar.Conversions.To_Unix_Time (p.periodstart.tm)'Img &
                                         " AND ts_start < " & Ada.Calendar.Conversions.To_Unix_Time (p.periodend.tm)'Img   &
                                         " order by ts_start";
   begin
      declare
         stmt2 : Stmt_Type_Local := DR.query (sql2);
      begin
         costs_numrows := stmt2.rows_returned;
         if ctime.tm_year > 1999 then
            null;
         end if;
         if costs_numrows > 0 then
            start_billing :
            loop
               costs_row := stmt2.fetch_next;
               if Interfaces.C.long (costs_row.column (3).as_nbyte8) = 9999999999  then
                  localperiodrec.periodend.tm  := p.periodend.tm;
               elsif p.periodend.tm  > Ada.Calendar.Conversions.To_Ada_Time (Interfaces.C.long (costs_row.column (3).as_nbyte8)) then
                  localperiodrec.periodend.tm  := Ada.Calendar.Conversions.To_Ada_Time (Interfaces.C.long (costs_row.column (3).as_nbyte8));
               else
                  localperiodrec.periodend.tm  := p.periodend.tm;
               end if;
               populate_subs (localperiodrec.periodend);
               standing_charge_rate := Long_Integer (costs_row.column (4).as_nbyte8);
               kwhr_rate            := Long_Integer (costs_row.column (5).as_nbyte8);
               VAT_rate             := Long_Integer (costs_row.column (6).as_nbyte8);
               declare
                  sql1 : constant String     := "SELECT count(count) from count where seconds > " & Ada.Calendar.Conversions.To_Unix_Time (localperiodrec.periodstart.tm)'Img &
                                       " AND seconds <= "                                & Ada.Calendar.Conversions.To_Unix_Time (localperiodrec.periodend.tm)'Img;
                  stmt1 : Stmt_Type_Local := DR.query (sql1);
               begin
                  row                     := stmt1.fetch_next;
                  kwhr                    := Long_Integer (row.column (1).as_nbyte8) * 4;
                  kwcostnet               := kwhr * kwhr_rate;
                  if pt = Record_Types.year then
                     days_billed          := Get_Days_In_Month (localperiodrec.periodstart, localperiodrec.periodend);
                     total_days_billed    := total_days_billed + days_billed;
                     sccostnet            := Long_Integer (days_billed) * standing_charge_rate;
                  elsif pt = Record_Types.month or else pt = Record_Types.offset  or else pt = Record_Types.day or else pt = Record_Types.week then
                     days_billed          := 1;
                     total_days_billed    := days_billed;
                     sccostnet            := standing_charge_rate;
                  end if;
                  usage.power_used        := usage.power_used + kwhr;
               end;
               kwcostgross                := kwcostnet         * VAT_rate;
               sccostgross                := sccostnet         * VAT_rate;
               usage.kwcostnet            := usage.kwcostnet   + kwcostnet;
               usage.kwcostgross          := usage.kwcostgross + kwcostgross;
               usage.sccostnet            := usage.sccostnet   + sccostnet;
               usage.sccostgross          := usage.sccostgross + sccostgross;
               exit start_billing when localperiodrec.periodend.tm = p.periodend.tm;
               localperiodrec.periodstart := localperiodrec.periodend;
            end loop start_billing;
         end if;
         usage.day_count      := total_days_billed;
         usage.period_details := p;
      end;
      return usage;
   end Get_Power_Usage;

   procedure Set_Account_Details (Host, DB, User, Pass : Ada.Strings.Unbounded.Unbounded_String; Port : Integer := 3306) is
   begin
      Database := DB;
      DB_Host  := Host;
      DB_User  := User;
      DB_Pass  := Pass;
      DB_Port  := Port;
   end Set_Account_Details;
end db_routines;
