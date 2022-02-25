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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Sets;
with Ada.Text_IO;
with Interfaces;              use Interfaces;
with Interfaces.C;
with Interfaces.C.Strings;    use Interfaces.C.Strings;
with Unix_Utils;
with Print_Line;
with Xstrings; use Xstrings;
with System; use System;

package body Generate_HTML is
   package UBS_Sets is new Ada.Containers.Ordered_Sets (Element_Type => Ada.Strings.Unbounded.Unbounded_String); use UBS_Sets;
   index_html_file : Ada.Text_IO.File_Type;
   indexstr        : Ada.Strings.Unbounded.Unbounded_String;
   S               : Set;

   procedure Store_Filename (FN : Directory_Entry_Type) is
   begin
      S.Insert (To_Unbounded_String (Ada.Directories.Simple_Name (FN)));
   end Store_Filename;

   procedure update is
      C          : Cursor;
      res        : Integer_32;
      im_html    : constant Unbounded_String := To_Unbounded_String ("im.html");
      Web_User   :          Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_String ("wwwrun");
      Web_PWD    :          Unix_Utils.Password_Entry;
      CHOwn_Res  :          Interfaces.Integer_32;
      Buffer_PWD :          String (1 .. 2 ** 16);
      Buffer_Len : constant Interfaces.Unsigned_64         := Buffer_PWD'Length;
      Result_PWD :          System.Address;
      C_Ptr      :          Interfaces.C.Strings.chars_ptr;
   begin
      if Unix_Utils.getuid = 0 then
         indexstr := dirstr_www;
      else
         indexstr := dirstr_usr;
      end if;

      Ada.Text_IO.Create (File => index_html_file, Name => To_String (indexstr & im_html));
      Ada.Text_IO.Put_Line (index_html_file, "<!DOCTYPE html PUBLIC ""-//W3C//DTD XHTML 1.0 Strict//EN"" ""http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd""/>");
      Ada.Text_IO.Put_Line (index_html_file, "  <html xmlns=""http://www.w3.org/1999/xhtml"" lang=""en"" dir=""ltr"" />");
      Ada.Text_IO.Put_Line (index_html_file, "    <head >");
      Ada.Text_IO.Put_Line (index_html_file, "      <meta http-equiv=""Content-Type"" content=""text/html; charset=iso-8859-1""/>");
      Ada.Text_IO.Put_Line (index_html_file, "      <title> SVG test </title>");
      Ada.Text_IO.Put_Line (index_html_file, "    </head>");
      Ada.Text_IO.Put_Line (index_html_file, "    <body>");

      Ada.Directories.Search (Directory => To_String (indexstr) & "year/power",
                              Pattern   => "20??_year.svg",
                              Filter    => (Ordinary_File => True, others => False),
                              Process   => Store_Filename'Access);
      write_year :
      for R of S loop
         Ada.Text_IO.Put_Line (index_html_file, "      <p>");
         Ada.Text_IO.Put_Line (index_html_file, "        <object data=""/svg/year/power/" & To_String (R) & """ type=""image/svg+xml""></object>");
         Ada.Text_IO.Put_Line (index_html_file, "      </p>");
      end loop write_year;

      S.Clear;
      Ada.Directories.Search (Directory => To_String (indexstr) & "month/power",
                              Pattern   => "20??_??_month.svg",
                              Filter    => (Ordinary_File => True, others => False),
                              Process   => Store_Filename'Access);
      C := Last (S);
      C := Previous (C);
      C := Previous (C);
      Ada.Text_IO.Put_Line (index_html_file, "      <p>");
      Ada.Text_IO.Put_Line (index_html_file, "        <object data=""/svg/month/power/" & To_String (Element (C)) & """ type=""image/svg+xml""></object>");
      Ada.Text_IO.Put_Line (index_html_file, "      </p>");
      C := Next (C);
      Ada.Text_IO.Put_Line (index_html_file, "      <p>");
      Ada.Text_IO.Put_Line (index_html_file, "        <object data=""/svg/month/power/" & To_String (Element (C)) & """ type=""image/svg+xml""></object>");
      Ada.Text_IO.Put_Line (index_html_file, "      </p>");
      C := Next (C);
      Ada.Text_IO.Put_Line (index_html_file, "      <p>");
      Ada.Text_IO.Put_Line (index_html_file, "        <object data=""/svg/month/power/" & To_String (Element (C)) & """ type=""image/svg+xml""></object>");
      Ada.Text_IO.Put_Line (index_html_file, "      </p>");
      S.Clear;
      Ada.Directories.Search (Directory => To_String (indexstr) & "offset/power",
                              Pattern   => "20??_??_offset.svg",
                              Filter    => (Ordinary_File => True, others => False),
                              Process   => Store_Filename'Access);
      C := Last (S);
      C := Previous (C);
      C := Previous (C);
      Ada.Text_IO.Put_Line (index_html_file, "      <p>");
      Ada.Text_IO.Put_Line (index_html_file, "        <object data=""/svg/offset/power/" & To_String (Element (C)) & """ type=""image/svg+xml""></object>");
      Ada.Text_IO.Put_Line (index_html_file, "      </p>");
      C := Next (C);
      Ada.Text_IO.Put_Line (index_html_file, "      <p>");
      Ada.Text_IO.Put_Line (index_html_file, "        <object data=""/svg/offset/power/" & To_String (Element (C)) & """ type=""image/svg+xml""></object>");
      Ada.Text_IO.Put_Line (index_html_file, "      </p>");
      C := Next (C);
      Ada.Text_IO.Put_Line (index_html_file, "      <p>");
      Ada.Text_IO.Put_Line (index_html_file, "        <object data=""/svg/offset/power/" & To_String (Element (C)) & """ type=""image/svg+xml""></object>");
      Ada.Text_IO.Put_Line (index_html_file, "      </p>");
      S.Clear;
--      Ada.Directories.Search (Directory => To_String (indexstr) & "week/power",
--                              Pattern   => "20??_??_??.svg",
--                              Filter    => (Ordinary_File => True, others => False),
--                              Process   => Store_Filename'Access);
--      C := Last (S);
--      C := Previous (C);
--      C := Previous (C);
--      Ada.Text_IO.Put_Line (index_html_file, "      <p>");
--      Ada.Text_IO.Put_Line (index_html_file, "        <object data=""/svg/month_offset/power/" & To_String (Element (C)) & """ type=""image/svg+xml""></object>");
--      Ada.Text_IO.Put_Line (index_html_file, "      </p>");
--      C := Next (C);
--      Ada.Text_IO.Put_Line (index_html_file, "      <p>");
--      Ada.Text_IO.Put_Line (index_html_file, "        <object data=""/svg/month_offset/power/" & To_String (Element (C)) & """ type=""image/svg+xml""></object>");
--      Ada.Text_IO.Put_Line (index_html_file, "      </p>");
--      C := Next (C);
--      Ada.Text_IO.Put_Line (index_html_file, "      <p>");
--      Ada.Text_IO.Put_Line (index_html_file, "        <object data=""/svg/month_offset/power/" & To_String (Element (C)) & """ type=""image/svg+xml""></object>");
--      Ada.Text_IO.Put_Line (index_html_file, "      </p>");
      S.Clear;
      Ada.Directories.Search (Directory => To_String (indexstr) & "day/power",
                              Pattern   => "20??_??_??_day.svg",
                              Filter    => (Ordinary_File => True, others => False),
                              Process   => Store_Filename'Access);
      C := Last (S);
      C := Previous (C);
      C := Previous (C);
      Ada.Text_IO.Put_Line (index_html_file, "      <p>");
      Ada.Text_IO.Put_Line (index_html_file, "        <object data=""/svg/day/power/" & To_String (Element (C)) & """ type=""image/svg+xml""></object>");
      Ada.Text_IO.Put_Line (index_html_file, "      </p>");
      C := Next (C);
      Ada.Text_IO.Put_Line (index_html_file, "      <p>");
      Ada.Text_IO.Put_Line (index_html_file, "        <object data=""/svg/day/power/" & To_String (Element (C)) & """ type=""image/svg+xml""></object>");
      Ada.Text_IO.Put_Line (index_html_file, "      </p>");
      C := Next (C);
      Ada.Text_IO.Put_Line (index_html_file, "      <p>");
      Ada.Text_IO.Put_Line (index_html_file, "        <object data=""/svg/day/power/" & To_String (Element (C)) & """ type=""image/svg+xml""></object>");
      Ada.Text_IO.Put_Line (index_html_file, "      </p>");

      Ada.Text_IO.Put_Line (index_html_file, "    </body>");
      Ada.Text_IO.Put_Line (index_html_file, "  </html>");
      Ada.Text_IO.Close (index_html_file);
      res := 0;

      if Unix_Utils.getuid = 0 then
         CHOwn_Res := Unix_Utils.Get_PW_Name_R (Web_User,
                                                Web_PWD'Address,
                                                Buffer_PWD'Address,
                                                Buffer_Len,
                                                Result_PWD'Address);

         if Result_PWD = System.Null_Address or else CHOwn_Res /= 0 then
            Ada.Text_IO.Put_Line ("Get_PW_Name_R failed, Null returned or CHOwn_Res not zero: " &
                                    Web_PWD.pw_uid'Image                                        &
                                    ", "                                                        &
                                    Web_PWD.pw_gid'Image                                        &
                                    ", "                                                        &
                                    CHOwn_Res'Image);
            return;
         end if;

         C_Ptr := Interfaces.C.Strings.New_String ((To_String (indexstr & im_html)));
         CHOwn_Res := Unix_Utils.chown (C_Ptr,
                                        Web_PWD.pw_uid,
                                        Web_PWD.pw_gid);
         Interfaces.C.Strings.Free (C_Ptr);

         if CHOwn_Res /= 0 then
            Ada.Text_IO.Put_Line ("chown failed: " &
                                    CHOwn_Res'Image);
            return;
         end if;
      end if;

      if res /= 0 then
         Print_Line.Print_Single_Line ("Chown failed on " & To_String (indexstr & im_html) & ", with result " & res'Img);
      end if;
   end update;
end Generate_HTML;
