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

with Ada.Text_IO;                    use Ada.Text_IO;
with GNAT.Calendar.Time_IO;
with Ada.Calendar.Conversions;
with Ada.Integer_Text_IO;
with Ada.Float_Text_IO;
with Ada.Characters.Latin_1;
with Conversion; use Conversion;

package body Print_Line is

   dt : constant GNAT.Calendar.Time_IO.Picture_String := "%c";
   procedure Print_Line_Day (t1 : Record_Types.tmrec) is
   begin
      GNAT.Calendar.Time_IO.Put_Time (t1.tm, dt);
      Put_Line ("");
      Ada.Text_IO.Flush;
   end Print_Line_Day;

   procedure Print_Line_Month (t1 : Record_Types.tmrec; t2 : Record_Types.tmrec) is
   begin
      GNAT.Calendar.Time_IO.Put_Time (t1.tm, dt);
      Put (" ");
      GNAT.Calendar.Time_IO.Put_Time (t2.tm, dt);
      Put (t1.tm_mon'Img);
      Ada.Text_IO.Put (Ada.Calendar.Conversions.To_Unix_Time (t2.tm)'Img);
      Put_Line ("");
      Ada.Text_IO.Flush;
   end Print_Line_Month;

   procedure Print_Line_Year (t1 : Record_Types.tmrec; t2 : Record_Types.tmrec; t3 : Record_Types.tmrec; t4 : Record_Types.tmrec) is
   begin
      GNAT.Calendar.Time_IO.Put_Time (t1.tm, dt);
      Put (" ");
      GNAT.Calendar.Time_IO.Put_Time (t2.tm, dt);
      Put (t1.tm_mon'Img);
      Ada.Text_IO.Put (Ada.Calendar.Conversions.To_Unix_Time (t2.tm)'Img);
      Put ("     OFFSET... ");
      GNAT.Calendar.Time_IO.Put_Time (t3.tm, dt);
      Put (" ");
      GNAT.Calendar.Time_IO.Put_Time (t4.tm, dt);
      Put (t4.tm_mon'Img);
      Ada.Text_IO.Put (Ada.Calendar.Conversions.To_Unix_Time (t4.tm)'Img);
      Put_Line ("");
      Ada.Text_IO.Flush;
   end Print_Line_Year;

   procedure Print_Power_Array (t1 : Record_Types.pow_array) is
   begin
      Put ("Index");
      Set_Col (20);
      Put ("Days");
      Set_Col (40);
      Put ("Power");
      Set_Col (60);
      Put ("Kw " & Character'Val (194) & Ada.Characters.Latin_1.Pound_Sign);
      Set_Col (80);
      Put ("Kw+V " & Character'Val (194) & Ada.Characters.Latin_1.Pound_Sign);
      Set_Col (100);
      Put ("SC " & Character'Val (194) & Ada.Characters.Latin_1.Pound_Sign);
      Set_Col (120);
      Put ("SC+V " & Character'Val (194) & Ada.Characters.Latin_1.Pound_Sign);
      Set_Col (140);
      Put ("Bill eVAT " & Character'Val (194) & Ada.Characters.Latin_1.Pound_Sign);
      Set_Col (160);
      Put ("Bill iVAT " & Character'Val (194) & Ada.Characters.Latin_1.Pound_Sign);
      Set_Col (180);
      Put ("Date start");
      Set_Col (210);
      Put ("Date end");
      Put_Line ("");
      for rindex in t1'Range loop
         Ada.Integer_Text_IO.Put (rindex);
         Set_Col (20);
         Ada.Integer_Text_IO.Put (Integer (t1 (rindex).day_count), 0);
         Set_Col (40);
         Ada.Float_Text_IO.Put (CF_power (t1 (rindex).power_used), 3, 5, 0);
         Set_Col (60);
         Ada.Float_Text_IO.Put (CF_kwcostnet (t1 (rindex).kwcostnet), 3, 5, 0);
         Set_Col (80);
         Ada.Float_Text_IO.Put (CF_kwcostgross (t1 (rindex).kwcostgross), 3, 4, 0);
         Set_Col (100);
         Ada.Float_Text_IO.Put (CF_sccostnet (t1 (rindex).sccostnet), 3, 4, 0);
         Set_Col (120);
         Ada.Float_Text_IO.Put (CF_sccostgross (t1 (rindex).sccostgross), 3, 4, 0);
         Set_Col (140);
         Ada.Float_Text_IO.Put (CF_kwcostnet (t1 (rindex).kwcostnet) + CF_sccostnet (t1 (rindex).sccostnet), 3, 4, 0);
         Set_Col (160);
         Ada.Float_Text_IO.Put (CF_kwcostgross (t1 (rindex).kwcostgross) + CF_sccostgross (t1 (rindex).sccostgross), 3, 4, 0);
         Set_Col (180);
         Put (GNAT.Calendar.Time_IO.Image (t1 (rindex).period_details.periodstart.tm, dt));
         Set_Col (210);
         Put (GNAT.Calendar.Time_IO.Image (t1 (rindex).period_details.periodend.tm, dt));
         Ada.Text_IO.Put_Line ("");
      end loop;
      Set_Col (40);
      Ada.Float_Text_IO.Put (CF_power (t1 (t1'Last).totalpower), 3, 5, 0);
      Set_Col (60);
      Ada.Float_Text_IO.Put (CF_kwcostnet (t1 (t1'Last).totalkwcostnet), 3, 4, 0);
      Set_Col (80);
      Ada.Float_Text_IO.Put (CF_kwcostgross (t1 (t1'Last).totalkwcostgross), 3, 4, 0);
      Set_Col (100);
      Ada.Float_Text_IO.Put (CF_sccostnet (t1 (t1'Last).totalsccostnet), 3, 4, 0);
      Set_Col (120);
      Ada.Float_Text_IO.Put (CF_sccostgross (t1 (t1'Last).totalsccostgross), 3, 4, 0);
      Set_Col (140);
      Ada.Float_Text_IO.Put (CF_kwcostnet (t1 (t1'Last).totalkwcostnet) + CF_sccostnet (t1 (t1'Last).totalsccostnet), 3, 5, 0);
      Set_Col (160);
      Ada.Float_Text_IO.Put (CF_kwcostgross (t1 (t1'Last).totalkwcostgross) + CF_sccostgross (t1 (t1'Last).totalsccostgross), 3, 4, 0);
      Ada.Text_IO.Put_Line ("");
      Ada.Text_IO.Flush;
   end Print_Power_Array;

   procedure Print_Single_Line (s : String) is
   begin
      Put_Line (s);
   end Print_Single_Line;

end Print_Line;
