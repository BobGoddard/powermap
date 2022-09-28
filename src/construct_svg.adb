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

with DOM.Core;
with DOM.Core.Documents;
with DOM.Core.Nodes;
with DOM.Core.Elements;
with Ada.Strings;             use Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;        use Ada.Strings.Maps;
with Ada.Streams.Stream_IO;
with Ada.Characters.Handling;
with Xstrings;                use Xstrings;
with Ada.Float_Text_IO;
with Ada.Integer_Text_IO;
with GNAT.Calendar;           use GNAT.Calendar;
with Conversion;              use Conversion;
with Interfaces;              use Interfaces;
with Interfaces.C;
with Interfaces.C.Strings;    use Interfaces.C.Strings;
with Unix_Utils;
with System; use System;

package body Construct_SVG is
   XL_Margin    : constant Integer := 100;
   XR_Margin    : constant Integer := 20;
   YT_Margin    : constant Integer := 30;
   Separation   : constant Integer := 46;
   First_Offset : constant Integer := 34;
   Bar_Width    : constant Integer := 26;
   My_Document  :          DOM.Core.Document;
   My_Root_Node :          DOM.Core.Element;
   divs         :          graph_divs;
   YB_Margin    :          Integer;
   svg_width    :          Integer;
   svg_height   :          Integer;

   procedure Add_Line (X1, X2 : Integer; Y1 : Float) is
      My_Line_Node : DOM.Core.Element;
   begin
      My_Line_Node  := DOM.Core.Documents.Create_Element (My_Document, "line");
      DOM.Core.Elements.Set_Attribute (My_Line_Node, "class", "major-axis");
      DOM.Core.Elements.Set_Attribute (My_Line_Node, "x1", Integer_To_String (X1));
      DOM.Core.Elements.Set_Attribute (My_Line_Node, "x2", Integer_To_String (X2));
      DOM.Core.Elements.Set_Attribute (My_Line_Node, "y1", Float_To_String   (Y1, 3));
      DOM.Core.Elements.Set_Attribute (My_Line_Node, "y2", Float_To_String   (Y1, 3));
      My_Line_Node := DOM.Core.Nodes.Append_Child (My_Root_Node, My_Line_Node);
   end Add_Line;

   procedure Add_Line_Dashed (X1, X2 : Integer; Y1 : Float) is
      My_Line_Node : DOM.Core.Element;
   begin
      My_Line_Node  := DOM.Core.Documents.Create_Element (My_Document, "line");
      DOM.Core.Elements.Set_Attribute (My_Line_Node, "class", "minor-axis");
      DOM.Core.Elements.Set_Attribute (My_Line_Node, "x1", Integer_To_String (X1));
      DOM.Core.Elements.Set_Attribute (My_Line_Node, "x2", Integer_To_String (X2));
      DOM.Core.Elements.Set_Attribute (My_Line_Node, "y1", Float_To_String   (Y1, 3));
      DOM.Core.Elements.Set_Attribute (My_Line_Node, "y2", Float_To_String   (Y1, 3));
      My_Line_Node := DOM.Core.Nodes.Append_Child (My_Root_Node, My_Line_Node);
   end Add_Line_Dashed;

   procedure Add_Min_Max_Tot (MinMax : Min_Max; Total : Long_Integer) is
      UStr         : Unbounded_String;
      FUStr        : Unbounded_String;
      Mf_min       : Float;
      Mf_max       : Float;
      TotalF       : Float;
      My_Text_Node : DOM.Core.Element;
      My_Text      : DOM.Core.Text;
   begin
      TotalF := CF_power (Total);
      Mf_min := CF_power (MinMax.min);
      Mf_max := CF_power (MinMax.max);

      UStr   := To_Unbounded_String ("Min: ");
      FUStr  := Float_To_UnBounded (Mf_min, 5);
      Ada.Strings.Unbounded.Append (UStr, FUStr & "kWh");
      My_Text_Node := DOM.Core.Documents.Create_Element (My_Document, "text");
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "x", "10");
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "y", Integer_To_String (svg_height - 10));
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "class", "def-black");
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "text-anchor", "left");
      My_Text_Node := DOM.Core.Nodes.Append_Child (My_Root_Node, My_Text_Node);
      My_Text      := DOM.Core.Documents.Create_Text_Node (My_Document, To_String (UStr));
      My_Text      := DOM.Core.Nodes.Append_Child (My_Text_Node, My_Text);

      UStr := To_Unbounded_String ("Max: ");
      FUStr  := Float_To_UnBounded (Mf_max, 5);
      Ada.Strings.Unbounded.Append (UStr, FUStr & "kWh");
      My_Text_Node := DOM.Core.Documents.Create_Element (My_Document, "text");
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "x", "10");
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "y", Integer_To_String (svg_height - 30));
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "class", "def-black");
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "text-anchor", "left");
      My_Text_Node := DOM.Core.Nodes.Append_Child (My_Root_Node, My_Text_Node);
      My_Text      := DOM.Core.Documents.Create_Text_Node (My_Document, To_String (UStr));
      My_Text      := DOM.Core.Nodes.Append_Child (My_Text_Node, My_Text);

      UStr := To_Unbounded_String ("Tot: ");
      FUStr  := Float_To_UnBounded (TotalF, 5);
      Ada.Strings.Unbounded.Append (UStr, FUStr & "kWh");
      My_Text_Node := DOM.Core.Documents.Create_Element (My_Document, "text");
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "x", "10");
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "y", Integer_To_String (svg_height - 50));
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "class", "def-black");
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "text-anchor", "left");
      My_Text_Node := DOM.Core.Nodes.Append_Child (My_Root_Node, My_Text_Node);
      My_Text      := DOM.Core.Documents.Create_Text_Node (My_Document, To_String (UStr));
      My_Text      := DOM.Core.Nodes.Append_Child (My_Text_Node, My_Text);
   end Add_Min_Max_Tot;

   procedure Add_Min_Max_Tot (MinMax : Min_Max; MinMax_Offset : Min_Max; Total : Long_Integer; Total_Offset : Long_Integer) is
      UStr         : Unbounded_String;
      FUStr1       : Unbounded_String;
      FUStr2       : Unbounded_String;
      Mf_min       : Float;
      Mf_max       : Float;
      Mf_off_min   : Float;
      Mf_off_max   : Float;
      TotalF       : Float;
      TotalFO      : Float;
      My_Text_Node : DOM.Core.Element;
      My_Text      : DOM.Core.Text;
   begin
      Mf_min     := CF_power (MinMax.min);
      Mf_off_min := CF_power (MinMax_Offset.min);
      Mf_max     := CF_power (MinMax.max);
      Mf_off_max := CF_power (MinMax_Offset.max);
      TotalF     := CF_power (Total);
      TotalFO    := CF_power (Total_Offset);

      UStr       := To_Unbounded_String ("Min: ");
      FUStr1     := Float_To_UnBounded (Mf_min, 5);
      FUStr2     := Float_To_UnBounded (Mf_off_min, 5);
      Ada.Strings.Unbounded.Append (UStr, FUStr1 & "/" & FUStr2 & "kWh");
      My_Text_Node := DOM.Core.Documents.Create_Element (My_Document, "text");
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "x", "10");
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "y", Integer_To_String (svg_height - 10));
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "class", "def-black");
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "text-anchor", "left");
      My_Text_Node := DOM.Core.Nodes.Append_Child (My_Root_Node, My_Text_Node);
      My_Text      := DOM.Core.Documents.Create_Text_Node (My_Document, To_String (UStr));
      My_Text      := DOM.Core.Nodes.Append_Child (My_Text_Node, My_Text);

      UStr         := To_Unbounded_String ("Max: ");
      FUStr1       := Float_To_UnBounded (Mf_max, 5);
      FUStr2       := Float_To_UnBounded (Mf_off_max, 5);
      Ada.Strings.Unbounded.Append (UStr, FUStr1 & "/" & FUStr2 & "kWh");
      My_Text_Node := DOM.Core.Documents.Create_Element (My_Document, "text");
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "x", "10");
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "y", Integer_To_String (svg_height - 30));
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "class", "def-black");
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "text-anchor", "left");
      My_Text_Node := DOM.Core.Nodes.Append_Child (My_Root_Node, My_Text_Node);
      My_Text      := DOM.Core.Documents.Create_Text_Node (My_Document, To_String (UStr));
      My_Text      := DOM.Core.Nodes.Append_Child (My_Text_Node, My_Text);

      UStr := To_Unbounded_String ("Tot: ");
      FUStr1       := Float_To_UnBounded (TotalF, 5);
      FUStr2       := Float_To_UnBounded (TotalFO, 5);
      Ada.Strings.Unbounded.Append (UStr, FUStr1 & "/" & FUStr2 & "kWh");
      My_Text_Node := DOM.Core.Documents.Create_Element (My_Document, "text");
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "x", "10");
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "y", Integer_To_String (svg_height - 50));
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "class", "def-black");
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "text-anchor", "left");
      My_Text_Node := DOM.Core.Nodes.Append_Child (My_Root_Node, My_Text_Node);
      My_Text      := DOM.Core.Documents.Create_Text_Node (My_Document, To_String (UStr));
      My_Text      := DOM.Core.Nodes.Append_Child (My_Text_Node, My_Text);
   end Add_Min_Max_Tot;

   procedure Add_Postamble is
      My_Script_Node      : DOM.Core.Element;
      My_CData_Node       : DOM.Core.Cdata_Section;
   begin
      My_Script_Node  := DOM.Core.Documents.Create_Element (My_Document, "script");
      DOM.Core.Elements.Set_Attribute (My_Script_Node, "type", "text/ecmascript");
      My_Script_Node  := DOM.Core.Nodes.Append_Child (My_Root_Node, My_Script_Node);

      My_CData_Node  := DOM.Core.Documents.Create_Cdata_Section (My_Document, cdata_str);
      My_CData_Node  := DOM.Core.Nodes.Append_Child (My_Script_Node, My_CData_Node);
   end Add_Postamble;

   procedure Add_Preamble is
      My_Style_Node       : DOM.Core.Element;
      My_Style_Text_Node  : DOM.Core.Text;
      My_Graph_Node       : DOM.Core.Element;
   begin
      My_Root_Node    := DOM.Core.Documents.Create_Element (My_Document, "svg");
      DOM.Core.Elements.Set_Attribute (My_Root_Node, "width",       Ada.Strings.Fixed.Trim (Integer'Image (svg_width),  Ada.Strings.Left));
      DOM.Core.Elements.Set_Attribute (My_Root_Node, "height",      Ada.Strings.Fixed.Trim (Integer'Image (svg_height), Ada.Strings.Left));
      DOM.Core.Elements.Set_Attribute (My_Root_Node, "onload",      "init(evt," &
                                         Ada.Strings.Fixed.Trim (Integer'Image (svg_width),  Ada.Strings.Left) & "," &
                                         Ada.Strings.Fixed.Trim (Integer'Image (svg_height), Ada.Strings.Left) & ")");
      DOM.Core.Elements.Set_Attribute (My_Root_Node, "xmlns",       "http://www.w3.org/2000/svg");
      DOM.Core.Elements.Set_Attribute (My_Root_Node, "xmlns:svg",   "http://www.w3.org/2000/svg");
      DOM.Core.Elements.Set_Attribute (My_Root_Node, "xmlns:xlink", "http://www.w3.org/1999/xlink");
      My_Root_Node    := DOM.Core.Nodes.Append_Child (My_Document, My_Root_Node);

      My_Style_Node  := DOM.Core.Documents.Create_Element (My_Document, "style");
      My_Style_Node  := DOM.Core.Nodes.Append_Child (My_Root_Node, My_Style_Node);

      My_Style_Text_Node    := DOM.Core.Documents.Create_Text_Node (My_Document, s1);
      My_Style_Text_Node    := DOM.Core.Nodes.Append_Child (My_Style_Node, My_Style_Text_Node);

      My_Graph_Node  := DOM.Core.Documents.Create_Element (My_Document, "rect");
      DOM.Core.Elements.Set_Attribute (My_Graph_Node, "fill", "#2080dd");
      DOM.Core.Elements.Set_Attribute (My_Graph_Node, "height", svg_height'Img);
      DOM.Core.Elements.Set_Attribute (My_Graph_Node, "rx", "10");
      DOM.Core.Elements.Set_Attribute (My_Graph_Node, "ry", "10");
      DOM.Core.Elements.Set_Attribute (My_Graph_Node, "width", svg_width'Img);
      DOM.Core.Elements.Set_Attribute (My_Graph_Node, "x", "0");
      DOM.Core.Elements.Set_Attribute (My_Graph_Node, "y", "0");
      My_Graph_Node  := DOM.Core.Nodes.Append_Child (My_Root_Node, My_Graph_Node);

      My_Graph_Node  := DOM.Core.Documents.Create_Element (My_Document, "rect");
      DOM.Core.Elements.Set_Attribute (My_Graph_Node, "fill", "blue");
      DOM.Core.Elements.Set_Attribute (My_Graph_Node, "height", Integer (divs.YDist + 2)'Img);
      DOM.Core.Elements.Set_Attribute (My_Graph_Node, "rx", "5");
      DOM.Core.Elements.Set_Attribute (My_Graph_Node, "ry", "5");
      DOM.Core.Elements.Set_Attribute (My_Graph_Node, "width", Integer (svg_width - (XL_Margin - 1) - (XR_Margin + 1))'Img);
      DOM.Core.Elements.Set_Attribute (My_Graph_Node, "x", Integer (XL_Margin - 1)'Img);
      DOM.Core.Elements.Set_Attribute (My_Graph_Node, "y", Ada.Strings.Fixed.Trim (Integer'Image (YT_Margin),  Ada.Strings.Left));
      DOM.Core.Elements.Set_Attribute (My_Graph_Node, "stroke-width", "2");
      DOM.Core.Elements.Set_Attribute (My_Graph_Node, "stroke", "#ff0000");
      My_Graph_Node  := DOM.Core.Nodes.Append_Child (My_Root_Node, My_Graph_Node);
   end Add_Preamble;

   procedure Add_Title       (Title : String) is
      My_Text_Node : DOM.Core.Element;
      My_Text      : DOM.Core.Text;
   begin
      My_Text_Node := DOM.Core.Documents.Create_Element (My_Document, "text");
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "x", Integer_To_String ((svg_width - XL_Margin - XR_Margin) / 2 + XL_Margin));
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "y", "20");
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "class", "def-black");
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "text-anchor", "middle");
      My_Text_Node := DOM.Core.Nodes.Append_Child (My_Root_Node, My_Text_Node);

      My_Text := DOM.Core.Documents.Create_Text_Node (My_Document, Title);
      My_Text := DOM.Core.Nodes.Append_Child (My_Text_Node, My_Text);
   end Add_Title;

   procedure Add_Tooltip_Area is
      My_Graph_Node       : DOM.Core.Element;
      My_Text_Node        : DOM.Core.Element;
      My_Text             : DOM.Core.Text;
   begin
      My_Graph_Node  := DOM.Core.Documents.Create_Element (My_Document, "rect");
      DOM.Core.Elements.Set_Attribute (My_Graph_Node, "class", "tooltip_bg");
      DOM.Core.Elements.Set_Attribute (My_Graph_Node, "id", "tooltip_bg");
      DOM.Core.Elements.Set_Attribute (My_Graph_Node, "visibility", "hidden");
      DOM.Core.Elements.Set_Attribute (My_Graph_Node, "x", "0");
      DOM.Core.Elements.Set_Attribute (My_Graph_Node, "y", "0");
      DOM.Core.Elements.Set_Attribute (My_Graph_Node, "width", "55");
      DOM.Core.Elements.Set_Attribute (My_Graph_Node, "height", "16");
      DOM.Core.Elements.Set_Attribute (My_Graph_Node, "rx", "5");
      DOM.Core.Elements.Set_Attribute (My_Graph_Node, "ry", "5");
      My_Graph_Node  := DOM.Core.Nodes.Append_Child (My_Root_Node, My_Graph_Node);

      My_Text_Node := DOM.Core.Documents.Create_Element (My_Document, "text");
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "class", "tooltip");
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "id", "tooltip_kwh");
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "visibility", "hidden");
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "x", "0");
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "y", "0");
      My_Text_Node := DOM.Core.Nodes.Append_Child (My_Root_Node, My_Text_Node);
      My_Text := DOM.Core.Documents.Create_Text_Node (My_Document, "wibble");
      My_Text := DOM.Core.Nodes.Append_Child (My_Text_Node, My_Text);

      My_Text_Node := DOM.Core.Documents.Create_Element (My_Document, "text");
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "class", "tooltip");
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "id", "tooltip_cost_kwh");
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "visibility", "hidden");
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "x", "0");
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "y", "0");
      My_Text_Node := DOM.Core.Nodes.Append_Child (My_Root_Node, My_Text_Node);
      My_Text := DOM.Core.Documents.Create_Text_Node (My_Document, "wibble");
      My_Text := DOM.Core.Nodes.Append_Child (My_Text_Node, My_Text);

      My_Text_Node := DOM.Core.Documents.Create_Element (My_Document, "text");
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "class", "tooltip");
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "id", "tooltip_cost_net");
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "visibility", "hidden");
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "x", "0");
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "y", "0");
      My_Text_Node := DOM.Core.Nodes.Append_Child (My_Root_Node, My_Text_Node);
      My_Text := DOM.Core.Documents.Create_Text_Node (My_Document, "wibble");
      My_Text := DOM.Core.Nodes.Append_Child (My_Text_Node, My_Text);

      My_Text_Node := DOM.Core.Documents.Create_Element (My_Document, "text");
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "class", "tooltip");
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "id", "tooltip_cost_gross");
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "visibility", "hidden");
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "x", "0");
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "y", "0");
      My_Text_Node := DOM.Core.Nodes.Append_Child (My_Root_Node, My_Text_Node);
      My_Text := DOM.Core.Documents.Create_Text_Node (My_Document, "wibble");
      My_Text := DOM.Core.Nodes.Append_Child (My_Text_Node, My_Text);

      My_Text_Node := DOM.Core.Documents.Create_Element (My_Document, "text");
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "class", "tooltip");
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "id", "tooltip_date");
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "visibility", "hidden");
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "x", "0");
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "y", "0");
      My_Text_Node := DOM.Core.Nodes.Append_Child (My_Root_Node, My_Text_Node);
      My_Text := DOM.Core.Documents.Create_Text_Node (My_Document, "wibble");
      My_Text := DOM.Core.Nodes.Append_Child (My_Text_Node, My_Text);
   end Add_Tooltip_Area;

   procedure Add_Usage (power_details : pow_array; uo : use_offset) is
      Rect_Node : DOM.Core.Element;
      ybar      : Float;
      all_str   : Ada.Strings.Unbounded.Unbounded_String;
   begin
      for i in power_details'Range loop
         if power_details (i).power_used /= 0 then
            all_str := Get_Tooltip (power_details (i), uo);
            ybar := Get_YBar (power_details (i).power_used);
            Rect_Node  := DOM.Core.Documents.Create_Element (My_Document, "rect");

            if uo = centre then
               DOM.Core.Elements.Set_Attribute (Rect_Node, "class", "data_green");
               DOM.Core.Elements.Set_Attribute (Rect_Node, "x", Integer_To_String (XL_Margin + Separation * i - Bar_Width));
            elsif uo = left then -- only applies to year
               DOM.Core.Elements.Set_Attribute (Rect_Node, "class", "data_green");
               DOM.Core.Elements.Set_Attribute (Rect_Node, "x", Integer_To_String (XL_Margin + Separation * i - Bar_Width - Separation / 6));
            else -- only applies to year
               DOM.Core.Elements.Set_Attribute (Rect_Node, "class", "data_red");
               DOM.Core.Elements.Set_Attribute (Rect_Node, "x", Integer_To_String (XL_Margin + Separation * i - Bar_Width + Separation / 6));
            end if;
            DOM.Core.Elements.Set_Attribute (Rect_Node, "height", Float_To_String (ybar + 1.0, 3));
            DOM.Core.Elements.Set_Attribute (Rect_Node, "width", Integer_To_String (Bar_Width));
            DOM.Core.Elements.Set_Attribute (Rect_Node, "y", Float_To_String (Float (YT_Margin + divs.YDist) - ybar, 3));
            DOM.Core.Elements.Set_Attribute (Rect_Node, "onmousemove", To_String (all_str));
            DOM.Core.Elements.Set_Attribute (Rect_Node, "onmouseout", "HideTooltip(evt)");
            Rect_Node  := DOM.Core.Nodes.Append_Child (My_Root_Node, Rect_Node);
         end if;
      end loop;
      null;
   end Add_Usage;

   procedure Add_Usage_Day (power_details : pow_array) is
      Rect_Node    : DOM.Core.Element;
      ybar         : Float;
      str          : Ada.Strings.Unbounded.Unbounded_String;
      str00        : Ada.Strings.Unbounded.Unbounded_String;
      str30        : Ada.Strings.Unbounded.Unbounded_String;
      Bar_Width_HR : constant Integer := 36;
   begin
      for D in power_details'Range loop
         if D mod 2 = 0 then
            if power_details (D - 1).power_used /= 0 and then power_details (D).power_used /= 0 then
               str := Get_Tooltip_Hour (power_details (D - 1), power_details (D));

               ybar := Get_YBar (power_details (D - 1).power_used + power_details (D).power_used);
               Rect_Node  := DOM.Core.Documents.Create_Element (My_Document, "rect");
               DOM.Core.Elements.Set_Attribute (Rect_Node, "class", "data_green");
               DOM.Core.Elements.Set_Attribute (Rect_Node, "x", Integer_To_String (XL_Margin + First_Offset + Separation * (D - 2) / 2 - Bar_Width_HR / 2));
               DOM.Core.Elements.Set_Attribute (Rect_Node, "height", Float_To_String (ybar + 1.0, 3));
               DOM.Core.Elements.Set_Attribute (Rect_Node, "width", Integer_To_String (Bar_Width_HR));
               DOM.Core.Elements.Set_Attribute (Rect_Node, "y", Float_To_String (Float (YT_Margin + divs.YDist) - ybar, 3));
               DOM.Core.Elements.Set_Attribute (Rect_Node, "onmousemove", To_String (str));
               DOM.Core.Elements.Set_Attribute (Rect_Node, "onmouseout", "HideTooltip(evt)");
               Rect_Node  := DOM.Core.Nodes.Append_Child (My_Root_Node, Rect_Node);
            end if;

            if power_details (D - 1).power_used /= 0 then
               str00 := Get_Tooltip (power_details (D - 1), centre);

               ybar := Get_YBar (power_details (D - 1).power_used);
               Rect_Node  := DOM.Core.Documents.Create_Element (My_Document, "rect");
               DOM.Core.Elements.Set_Attribute (Rect_Node, "class", "data_orange");
               DOM.Core.Elements.Set_Attribute (Rect_Node, "x", Integer_To_String (XL_Margin + First_Offset + Separation * (D - 2) / 2 - Bar_Width_HR / 2));
               DOM.Core.Elements.Set_Attribute (Rect_Node, "height", Float_To_String (ybar + 1.0, 3));
               DOM.Core.Elements.Set_Attribute (Rect_Node, "width", Integer_To_String (Bar_Width_HR / 2));
               DOM.Core.Elements.Set_Attribute (Rect_Node, "y", Float_To_String (Float (YT_Margin + divs.YDist) - ybar, 3));
               DOM.Core.Elements.Set_Attribute (Rect_Node, "onmousemove", To_String (str00));
               DOM.Core.Elements.Set_Attribute (Rect_Node, "onmouseout", "HideTooltip(evt)");
               Rect_Node  := DOM.Core.Nodes.Append_Child (My_Root_Node, Rect_Node);
            end if;

            if power_details (D).power_used /= 0 then
               str30 := Get_Tooltip (power_details (D), centre);

               ybar := Get_YBar (power_details (D).power_used);
               Rect_Node  := DOM.Core.Documents.Create_Element (My_Document, "rect");
               DOM.Core.Elements.Set_Attribute (Rect_Node, "class", "data_red");
               DOM.Core.Elements.Set_Attribute (Rect_Node, "x", Integer_To_String (XL_Margin + First_Offset + Separation * (D - 2) / 2));
               DOM.Core.Elements.Set_Attribute (Rect_Node, "height", Float_To_String (ybar + 1.0, 3));
               DOM.Core.Elements.Set_Attribute (Rect_Node, "width", Integer_To_String (Bar_Width_HR / 2));
               DOM.Core.Elements.Set_Attribute (Rect_Node, "y", Float_To_String (Float (YT_Margin + divs.YDist) - ybar, 3));
               DOM.Core.Elements.Set_Attribute (Rect_Node, "onmousemove", To_String (str30));
               DOM.Core.Elements.Set_Attribute (Rect_Node, "onmouseout", "HideTooltip(evt)");
               Rect_Node  := DOM.Core.Nodes.Append_Child (My_Root_Node, Rect_Node);
            end if;
         end if;
      end loop;
   end Add_Usage_Day;

   procedure Add_X_Axis_Day (power_details : pow_array) is
      My_Text_Node : DOM.Core.Element;
      My_Text      : DOM.Core.Text;
   begin
      H_Loop :
      for D in power_details'Range loop
         if power_details (D).period_details.periodstart.tm_minute = 30 then
            My_Text_Node := DOM.Core.Documents.Create_Element (My_Document, "text");
            DOM.Core.Elements.Set_Attribute (My_Text_Node, "x", Integer_To_String (XL_Margin + First_Offset + Separation * (D - 2) / 2)); -- M'Enum_Rep
            DOM.Core.Elements.Set_Attribute (My_Text_Node, "y", Integer_To_String (YT_Margin + divs.YDist + 20));
            DOM.Core.Elements.Set_Attribute (My_Text_Node, "class", "def-black");
            DOM.Core.Elements.Set_Attribute (My_Text_Node, "text-anchor", "middle");
            My_Text_Node := DOM.Core.Nodes.Append_Child (My_Root_Node, My_Text_Node);

            My_Text := DOM.Core.Documents.Create_Text_Node (My_Document, power_details (D).period_details.periodstart.tm_hour'Img);
            My_Text := DOM.Core.Nodes.Append_Child (My_Text_Node, My_Text);
         end if;
      end loop H_Loop;
   end Add_X_Axis_Day;

   procedure Add_X_Axis_Month (power_details : pow_array) is
      My_Text_Node : DOM.Core.Element;
      My_Text      : DOM.Core.Text;
   begin
      M_Loop :
      for D in power_details'Range loop
         My_Text_Node := DOM.Core.Documents.Create_Element (My_Document, "text");
         DOM.Core.Elements.Set_Attribute (My_Text_Node, "x", Integer_To_String (XL_Margin + First_Offset + Separation * (D - 1))); -- M'Enum_Rep
         DOM.Core.Elements.Set_Attribute (My_Text_Node, "y", Integer_To_String (YT_Margin + divs.YDist + 20));
         if power_details (D).period_details.periodstart.tm_wday = GNAT.Calendar.Saturday or else power_details (D).period_details.periodstart.tm_wday = GNAT.Calendar.Sunday then
            DOM.Core.Elements.Set_Attribute (My_Text_Node, "class", "def-red");
         else
            DOM.Core.Elements.Set_Attribute (My_Text_Node, "class", "def-black");
         end if;
         DOM.Core.Elements.Set_Attribute (My_Text_Node, "text-anchor", "middle");
         My_Text_Node := DOM.Core.Nodes.Append_Child (My_Root_Node, My_Text_Node);

         My_Text := DOM.Core.Documents.Create_Text_Node (My_Document, power_details (D).period_details.periodstart.tm_mday'Img);
         My_Text := DOM.Core.Nodes.Append_Child (My_Text_Node, My_Text);

      end loop M_Loop;
   end Add_X_Axis_Month;

   procedure Add_X_Axis_Year is
      My_Text_Node : DOM.Core.Element;
      My_Text      : DOM.Core.Text;
   begin
      Y_Loop :
      for M in 1 .. 12 loop
         My_Text_Node := DOM.Core.Documents.Create_Element (My_Document, "text");
         DOM.Core.Elements.Set_Attribute (My_Text_Node, "x", Integer_To_String (XL_Margin + First_Offset + Separation * (M - 1)));
         DOM.Core.Elements.Set_Attribute (My_Text_Node, "y", Integer_To_String (YT_Margin + divs.YDist + 20)); -- "301");
         DOM.Core.Elements.Set_Attribute (My_Text_Node, "class", "def-black");
         DOM.Core.Elements.Set_Attribute (My_Text_Node, "text-anchor", "middle");
         My_Text_Node := DOM.Core.Nodes.Append_Child (My_Root_Node, My_Text_Node);

         My_Text := DOM.Core.Documents.Create_Text_Node (My_Document, Month_Names (M));
         My_Text := DOM.Core.Nodes.Append_Child (My_Text_Node, My_Text);

      end loop Y_Loop;
   end Add_X_Axis_Year;

   procedure Add_Y_Axis is
      YLine      : Integer := 0;
      YDist_Line : Float;
   begin
      --  return CF_power (p) * Float (divs.YDist) / divs.Max;
      YLine_Loop :
      while YLine <= divs.NoDivs loop
         YDist_Line := Float (YT_Margin + divs.YDist) - Float (divs.YDist * YLine / divs.NoDivs);

         if YLine mod divs.Major = 0 then
            if YLine /= 0  and then YLine /= divs.NoDivs then
               Add_Line (XL_Margin - 5, svg_width - XR_Margin - 2, YDist_Line);
            end if;
            Add_Y_Axis_Label (XL_Margin - 5, Integer (YDist_Line) + 6, Float (YLine) * divs.Max / Float (divs.NoDivs));
         elsif YLine /= 0  and then YLine /= divs.NoDivs then
            Add_Line_Dashed (XL_Margin, svg_width - XR_Margin - 2, YDist_Line);
         end if;

         YLine := YLine + 1;
      end loop YLine_Loop;
   end Add_Y_Axis;

   procedure Add_Y_Axis_Label (X, Y : Integer; V : Float) is
      My_Text_Node : DOM.Core.Element;
      My_Text      : DOM.Core.Text;
   begin
      My_Text_Node := DOM.Core.Documents.Create_Element (My_Document, "text");
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "x", Integer_To_String (X - 1));
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "y", Integer_To_String (Y));
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "class", "def-black");
      DOM.Core.Elements.Set_Attribute (My_Text_Node, "text-anchor", "end");
      My_Text_Node := DOM.Core.Nodes.Append_Child (My_Root_Node, My_Text_Node);

      My_Text := DOM.Core.Documents.Create_Text_Node (My_Document, Float_To_String (V, 3));
      My_Text := DOM.Core.Nodes.Append_Child (My_Text_Node, My_Text);
   end Add_Y_Axis_Label;

   procedure Add_Y_Axis_Name (Y_String : String) is
      My_Text_Rotate_Node : DOM.Core.Element;
      My_Text             : DOM.Core.Text;
   begin
      My_Text_Rotate_Node := DOM.Core.Documents.Create_Element (My_Document, "text");
      DOM.Core.Elements.Set_Attribute (My_Text_Rotate_Node, "x", Integer_To_String (-(YT_Margin + divs.YDist / 2))); -- "-155");
      DOM.Core.Elements.Set_Attribute (My_Text_Rotate_Node, "y", "20");
      DOM.Core.Elements.Set_Attribute (My_Text_Rotate_Node, "class", "def-black");
      DOM.Core.Elements.Set_Attribute (My_Text_Rotate_Node, "text-anchor", "middle");
      DOM.Core.Elements.Set_Attribute (My_Text_Rotate_Node, "transform", "rotate(-90)");
      My_Text_Rotate_Node := DOM.Core.Nodes.Append_Child (My_Root_Node, My_Text_Rotate_Node);

      My_Text := DOM.Core.Documents.Create_Text_Node (My_Document, Y_String);
      My_Text := DOM.Core.Nodes.Append_Child (My_Text_Rotate_Node, My_Text);
   end Add_Y_Axis_Name;

   function Float_To_String (f : Float; A : Integer) return String is
      US : Ada.Strings.Unbounded.Unbounded_String;
      S  : String := "                          ";
      LSet1 : constant Character_Set := To_Set (' ');
      RSet1 : constant Character_Set := To_Set ("0");
      RSet2 : constant Character_Set := To_Set ('.');
   begin
      Ada.Float_Text_IO.Put (S, f, Aft => A, Exp => 0);
      US := To_Unbounded_String (S);
      Ada.Strings.Unbounded.Trim (US, LSet1, RSet1);
      Ada.Strings.Unbounded.Trim (US, LSet1, RSet2);
      return To_String (US);
   end Float_To_String;

   function Float_To_UnBounded (f : Float; A : Integer) return Ada.Strings.Unbounded.Unbounded_String is
      US : Ada.Strings.Unbounded.Unbounded_String;
      S     : String := "                          ";
      LSet1 : constant Character_Set := To_Set (' ');
      RSet1 : constant Character_Set := To_Set ("0");
      RSet2 : constant Character_Set := To_Set ('.');
   begin
      Ada.Float_Text_IO.Put (S, f, Aft => A, Exp => 0);
      US := To_Unbounded_String (S);
      Ada.Strings.Unbounded.Trim (US, LSet1, RSet1);
      Ada.Strings.Unbounded.Trim (US, LSet1, RSet2);
      return US;
   end Float_To_UnBounded;

   procedure Get_Graph_Extents (Max : Long_Integer; ldivs : out graph_divs) is
   begin
      ldivs := gd (1);

      div_loop :
      for i in gd'Range loop
         ldivs := gd (i);
         exit div_loop when gd (i).Max > CF_power (Max);
      end loop div_loop;
   end Get_Graph_Extents;

   function Get_Min_Max (power_details : pow_array) return Min_Max is
      MM : Min_Max;
   begin
      MM.min := Long_Integer'Last;
      MM.max := Long_Integer'First;
      for i in power_details'Range loop
         if power_details (i).power_used /= 0 and then power_details (i).power_used < MM.min then
            MM.min := power_details (i).power_used;
         end if;
         if power_details (i).power_used > MM.max then
            MM.max := power_details (i).power_used;
         end if;
      end loop;
      return MM;
   end Get_Min_Max;

   function Get_Min_Max_Day (power_details : pow_array) return Min_Max is
      m30 : Long_Integer;
      MM  : Min_Max;
   begin
      MM.min := Long_Integer'Last;
      MM.max := Long_Integer'First;
      for i in power_details'Range loop
         if power_details (i).period_details.periodstart.tm_minute = 0 then
            m30 := power_details (i).power_used;
         else
            if power_details (i).power_used /= 0 and then power_details (i).power_used  + m30 < MM.min then
               MM.min := power_details (i).power_used + m30;
            end if;
            if power_details (i).power_used + m30 > MM.max then
               MM.max := power_details (i).power_used + m30;
            end if;
         end if;
      end loop;
      return MM;
   end Get_Min_Max_Day;

   function Get_Tooltip (p : powerrec; uo : use_offset) return Unbounded_String is
      l_str_u : Unbounded_String;
      r_str_u : Unbounded_String;
      all_str : Unbounded_String;
   begin
      Ada.Strings.Unbounded.Set_Unbounded_String (all_str, "ShowTooltip(evt, '");

      l_str_u := Float_To_UnBounded (CF_power (p.power_used), 5);
      r_str_u := Float_To_UnBounded (CF_power (p.totalpower), 5);
      Append (all_str, l_str_u & "/" & r_str_u & "kWh");
      Append (all_str, "', '");

      l_str_u := Float_To_UnBounded (CF_kwcostnet (p.kwcostnet), 5);
      r_str_u := Float_To_UnBounded (CF_kwcostnet (p.totalkwcostnet), 5);
      Append (all_str, "£" & l_str_u & "/£" & r_str_u);
      Append (all_str, "', '");

      l_str_u := Float_To_UnBounded (CF_kwcostnet (p.kwcostnet) + CF_sccostnet (p.sccostnet), 5);
      r_str_u := Float_To_UnBounded (CF_kwcostnet (p.totalkwcostnet) + CF_sccostnet (p.totalsccostnet), 5);
      Append (all_str, "£" & l_str_u & "/£" & r_str_u);
      Append (all_str, "'");

      Append (all_str, ", '");
      l_str_u := Float_To_UnBounded (CF_kwcostgross (p.kwcostgross) + CF_sccostgross (p.sccostgross), 5);
      r_str_u := Float_To_UnBounded (CF_kwcostgross (p.totalkwcostgross) + CF_sccostgross (p.totalsccostgross), 5);
      Append (all_str, "£" & l_str_u & "/£" & r_str_u);
      Append (all_str, "'");

      if uo = right then
         Append (all_str, ", '");
         if p.period_details.periodstart.tm_mday <= 9 then
            Append (all_str, HR3_Whistle (p.period_details.periodstart.tm_mday) & " - ");
         else
            Append (all_str, HR4_Whistle (p.period_details.periodstart.tm_mday) & " - ");
         end if;

         if p.period_details.periodend.tm_mday <= 9 then
            Append (all_str, HR3_Whistle (p.period_details.periodend.tm_mday - 1));
         else
            Append (all_str, HR4_Whistle (p.period_details.periodend.tm_mday - 1));
         end if;
         Append (all_str, "'");
      else
         Append (all_str, ", 'undefined'");
      end if;

      Append (all_str, ")");
      return all_str;
   end Get_Tooltip;

   function Get_Tooltip_Hour (p1, p2 : powerrec) return Unbounded_String is
      l_str_u : Unbounded_String;
      r_str_u : Unbounded_String;
      all_str : Unbounded_String;
   begin
      Ada.Strings.Unbounded.Set_Unbounded_String (all_str, "ShowTooltip(evt, '");

      l_str_u := Float_To_UnBounded (CF_power (p1.power_used + p2.power_used), 5);
      r_str_u := Float_To_UnBounded (CF_power (p2.totalpower), 5);
      Append (all_str, l_str_u & "/" & r_str_u & "kWh");

      l_str_u := Float_To_UnBounded (CF_kwcostnet (p1.kwcostnet + p2.kwcostnet), 5);
      r_str_u := Float_To_UnBounded (CF_kwcostnet (p2.totalkwcostnet), 5);
      Append (all_str, "', '£" & l_str_u & "/£" & r_str_u);

      l_str_u := Float_To_UnBounded (CF_kwcostnet (p1.kwcostnet + p2.kwcostnet) + CF_sccostnet (p1.sccostnet + p2.sccostnet), 5);
      r_str_u := Float_To_UnBounded (CF_kwcostnet (p2.totalkwcostnet) + CF_sccostnet (p2.totalsccostnet), 5);
      Append (all_str, "', '£" & l_str_u & "/£" & r_str_u);

      l_str_u := Float_To_UnBounded (CF_kwcostgross (p1.kwcostgross + p2.kwcostgross) + CF_sccostgross (p1.sccostgross + p2.sccostgross), 5);
      r_str_u := Float_To_UnBounded (CF_kwcostgross (p2.totalkwcostgross) + CF_sccostgross (p2.totalsccostgross), 5);
      Append (all_str, "', '£" & l_str_u & "/£" & r_str_u & "', 'undefined')");

      return all_str;
   end Get_Tooltip_Hour;

   function  Get_YBar (p : Long_Integer) return Float is
   begin
      return CF_power (p) * Float (divs.YDist) / divs.Max;
   end Get_YBar;

   function Integer_To_String (I : Integer) return String is
      US : Ada.Strings.Unbounded.Unbounded_String;
      S  : String := "                          ";
   begin
      Ada.Integer_Text_IO.Put (S, I);
      US := To_Unbounded_String (S);
      Ada.Strings.Unbounded.Trim (US, Ada.Strings.Both);
      return To_String (US);
   end Integer_To_String;

   procedure Write_SVG (power_details, power_details_off : pow_array) is
      My_Implementation :          DOM.Core.DOM_Implementation;
      svg_file          :          Ada.Streams.Stream_IO.File_Type;
      MM                :          Min_Max;
      MM_Offset         :          Min_Max;
      uo                :          use_offset;
      dirstr            :          Unbounded_String;
      fname             :          Unbounded_String;
      res               :          Integer_32;
      pd                :          periodrec;
      Web_User          :          Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_String ("wwwrun");
      Web_PWD           :          Unix_Utils.Password_Entry;
      CHOwn_Res         :          Interfaces.Integer_32;
      Buffer_PWD        :          String (1 .. 2 ** 16);
      Buffer_Len        : constant Interfaces.Unsigned_64         := Buffer_PWD'Length;
      Result_PWD        :          System.Address;
      C_Ptr             :          Interfaces.C.Strings.chars_ptr;
   begin
      --      DOM.Core.Standalone(My_Implementation, True);
      if Unix_Utils.getuid = 0 then
         dirstr := dirstr_www & "year/power/";
      else
         dirstr := dirstr_usr & "year/power/";
      end if;

      YB_Margin := 100;
      MM        := Get_Min_Max (power_details);
      MM_Offset := Get_Min_Max (power_details_off);

      if MM.max > MM_Offset.max then
         Get_Graph_Extents (MM.max, divs);
      else
         Get_Graph_Extents (MM_Offset.max, divs);
      end if;

      pd := power_details (1).period_details;
      svg_height  := YT_Margin + divs.YDist + YB_Margin;
      svg_width   := XL_Margin + 580 + XR_Margin;
      My_Document := DOM.Core.Create_Document (My_Implementation);
      Add_Preamble;
      Add_Postamble;
      Add_X_Axis_Year;
      Add_Y_Axis_Name ("Power Usage kWh");
      Add_Y_Axis;
      uo := right;
      Add_Usage       (power_details_off, uo);
      uo := left;
      Add_Usage       (power_details, uo);
      Add_Title       (pd.periodstart.tm_year'Img);
      Add_Min_Max_Tot (MM, MM_Offset, power_details (power_details'Last).totalpower, power_details_off (power_details_off'Last).totalpower);
      Add_Tooltip_Area;
      fname := Ada.Strings.Unbounded.Trim (To_Unbounded_String (pd.periodstart.tm_year'Img), Ada.Strings.Left) & "_year.svg";
      Ada.Streams.Stream_IO.Create (File => svg_file, Name => To_String (dirstr & fname));

      DOM.Core.Nodes.Write (Ada.Streams.Stream_IO.Stream (svg_file),
                            N => My_Document,
                            Print_XML_Declaration => True,
                            With_URI              => False,
                            Pretty_Print          => True);

      Ada.Streams.Stream_IO.Close (svg_file);
      res := 0;

      if Unix_Utils.getuid = 0 then
         CHOwn_Res := Unix_Utils.Get_PW_Name_R (Web_User,
                                                Web_PWD'Address,
                                                Buffer_PWD'Address,
                                                Buffer_Len,
                                                Result_PWD'Address);

         if Result_PWD = System.Null_Address or else CHOwn_Res /= 0 then
            raise Program_Error with "Get_PW_Name_R failed, Null returned or CHOwn_Res not zero: " &
                                       Web_PWD.pw_uid'Image                                        &
                                       ", "                                                        &
                                       Web_PWD.pw_gid'Image                                        &
                                       ", "                                                        &
                                       CHOwn_Res'Image;
            return;
         end if;

         C_Ptr := Interfaces.C.Strings.New_String ((To_String (dirstr & fname)));
         CHOwn_Res := Unix_Utils.chown (C_Ptr,
                                        Web_PWD.pw_uid,
                                        Web_PWD.pw_gid);
         Interfaces.C.Strings.Free (C_Ptr);

         if CHOwn_Res /= 0 then
            raise Program_Error with "chown failed: " & CHOwn_Res'Image;
            return;
         end if;
      end if;

      if res /= 0 then
         raise Program_Error with "Chown failed on " & To_String (dirstr & fname) & ", with result " & res'Img;
      end if;
   end Write_SVG;

   procedure Write_SVG (power_details : pow_array; rtype : graph_period_type) is
      My_Implementation :          DOM.Core.DOM_Implementation;
      svg_file          :          Ada.Streams.Stream_IO.File_Type;
      MM                :          Min_Max;
      ds                :          String := "00";
      ms                :          String := "00";
      dirstr            :          Unbounded_String;
      fname             :          Unbounded_String;
      title             :          Unbounded_String;
      res               :          Integer_32;
      pd                :          periodrec;
      Web_User          :          Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_String ("wwwrun");
      Web_PWD           :          Unix_Utils.Password_Entry;
      CHOwn_Res         :          Interfaces.Integer_32;
      Buffer_PWD        :          String (1 .. 2 ** 16);
      Buffer_Len        : constant Interfaces.Unsigned_64         := Buffer_PWD'Length;
      Result_PWD        :          System.Address;
      C_Ptr             :          Interfaces.C.Strings.chars_ptr;
   begin
      --      DOM.Core.Standalone(My_Implementation, True);
      if Unix_Utils.getuid = 0 then
         dirstr := dirstr_www;
      else
         dirstr := dirstr_usr;
      end if;

      if rtype = day then
         MM        := Get_Min_Max_Day (power_details);
         svg_width   := XL_Margin + Separation * power_details'Length / 2 + Bar_Width + XR_Margin;
      else
         MM        := Get_Min_Max (power_details);
         svg_width   := XL_Margin + Separation * power_details'Length + Bar_Width + XR_Margin;
      end if;

      Append (dirstr, Ada.Characters.Handling.To_Lower (rtype'Img) & "/power/");
      YB_Margin := 100;
      Get_Graph_Extents (MM.max, divs);
      svg_height  := YT_Margin + divs.YDist + YB_Margin;
      My_Document := DOM.Core.Create_Document (My_Implementation);
      Add_Preamble;
      Add_Postamble;
      Add_Y_Axis_Name ("Power Usage kWh");
      Add_Y_Axis;
      Add_Min_Max_Tot (MM, power_details (power_details'Last).totalpower);
      pd := power_details (1).period_details;

      if rtype = month or else rtype = offset or else rtype = week then
         Add_X_Axis_Month (power_details);
         Add_Title        (Month_Names (pd.periodstart.tm_mon) & " " & pd.periodstart.tm_year'Img);
         Add_Usage        (power_details, Record_Types.centre);
      elsif rtype = day then
         Add_X_Axis_Day (power_details);
         if pd.periodstart.tm_mday <= 9 then
            title := To_Unbounded_String (HR3_Whistle (pd.periodstart.tm_mday));
         else
            title := To_Unbounded_String (HR4_Whistle (pd.periodstart.tm_mday));
         end if;
         Add_Title (To_String (title) & " " & Month_Names (pd.periodstart.tm_mon) & " " & pd.periodstart.tm_year'Img);
         Add_Usage_Day (power_details);
      end if;

      if pd.periodstart.tm_mon < 10 then
         ms := "0" & Ada.Strings.Fixed.Trim (pd.periodstart.tm_mon'Img, Ada.Strings.Both);
      else
         ms := Ada.Strings.Fixed.Trim (pd.periodstart.tm_mon'Img, Ada.Strings.Both);
      end if;

      if pd.periodstart.tm_mday < 10 then
         ds := "0" & Ada.Strings.Fixed.Trim (pd.periodstart.tm_mday'Img, Ada.Strings.Both);
      else
         ds := Ada.Strings.Fixed.Trim (pd.periodstart.tm_mday'Img, Ada.Strings.Both);
      end if;

      if rtype = month then
         fname := Ada.Strings.Unbounded.Trim (To_Unbounded_String (pd.periodstart.tm_year'Img), Ada.Strings.Left) & "_" & ms & "_month.svg";
      elsif rtype = offset then
         fname := Ada.Strings.Unbounded.Trim (To_Unbounded_String (pd.periodstart.tm_year'Img), Ada.Strings.Left) & "_" & ms & "_offset.svg";
      elsif rtype = week then
         fname := Ada.Strings.Unbounded.Trim (To_Unbounded_String (pd.periodstart.tm_year'Img), Ada.Strings.Left) & "_" & ms & "_" & ds & "_week.svg";
      elsif rtype = day then
         fname := Ada.Strings.Unbounded.Trim (To_Unbounded_String (pd.periodstart.tm_year'Img), Ada.Strings.Left) & "_" & ms & "_" & ds & "_day.svg";
      end if;

      Add_Tooltip_Area;
      Ada.Streams.Stream_IO.Create (File => svg_file, Name => To_String (dirstr & fname));
      DOM.Core.Nodes.Write (Ada.Streams.Stream_IO.Stream (svg_file),
                            N                     => My_Document,
                            Print_XML_Declaration => True,
                            With_URI              => False,
                            Pretty_Print          => True);
      Ada.Streams.Stream_IO.Close (svg_file);
      res := 0;

      if Unix_Utils.getuid = 0 then
         CHOwn_Res := Unix_Utils.Get_PW_Name_R (Web_User,
                                                Web_PWD'Address,
                                                Buffer_PWD'Address,
                                                Buffer_Len,
                                                Result_PWD'Address);

         if Result_PWD = System.Null_Address or else CHOwn_Res /= 0 then
            raise Program_Error with "Get_PW_Name_R failed, Null returned or CHOwn_Res not zero: " &
                                       Web_PWD.pw_uid'Image                                        &
                                       ", "                                                        &
                                       Web_PWD.pw_gid'Image                                        &
                                       ", "                                                        &
                                       CHOwn_Res'Image;
            return;
         end if;

         C_Ptr := Interfaces.C.Strings.New_String ((To_String (dirstr & fname)));
         CHOwn_Res := Unix_Utils.chown (C_Ptr,
                                        Web_PWD.pw_uid,
                                        Web_PWD.pw_gid);
         Interfaces.C.Strings.Free (C_Ptr);

         if CHOwn_Res /= 0 then
            raise Program_Error with "chown failed: " & CHOwn_Res'Image;
            return;
         end if;
      end if;

      if res /= 0 then
         raise Program_Error with "Chown failed on " & To_String (dirstr & fname) & ", with result " & res'Img;
      end if;
   end Write_SVG;

end Construct_SVG;
