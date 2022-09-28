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

with Ada.Command_Line;
with Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;
with GNAT.Traceback;
with GNAT.Traceback.Symbolic;
with GNATCOLL.Traces.Syslog;

package body Logger is
   Log_To_Syslog : Boolean := False;
   Log_To_stdout : Boolean := True;

   procedure Initialise is
   begin
      Log_To_stdout := True;
      Log_To_Syslog := False;

      if Log_To_Syslog then
         GNATCOLL.Traces.Syslog.Register_Syslog_Stream;
         GNATCOLL.Traces.Syslog.Openlog (Ada.Directories.Simple_Name (Ada.Command_Line.Command_Name), GNATCOLL.Traces.Syslog.None, GNATCOLL.Traces.Syslog.Daemon);
      end if;
   end Initialise;

   protected body Secure is
      procedure Logger (S : String) is
      begin
         if Log_To_Syslog then
            GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, S);
         end if;

         if Log_To_stdout then
            Ada.Text_IO.Put_Line (S);
         end if;
      end Logger;

      procedure Logger (S : Ada.Strings.Unbounded.Unbounded_String) is
      begin
         if Log_To_Syslog then
            GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, Ada.Strings.Unbounded.To_String (S));
         end if;

         if Log_To_stdout then
            Ada.Strings.Unbounded.Text_IO.Put_Line (S);
         end if;
      end Logger;

      procedure Logger (S : String;                                 E : Ada.Exceptions.Exception_Occurrence) is
         Trace        : GNAT.Traceback.Tracebacks_Array (1 .. 100);
         Length       : Natural;
      begin
         GNAT.Traceback.Call_Chain (Trace, Length);

         if Log_To_Syslog then
            GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, S & " - " & Ada.Exceptions.Exception_Name (E) & " message: " & Ada.Exceptions.Exception_Message (E));
            GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
         end if;

         if Log_To_stdout then
            Ada.Text_IO.Put_Line (S & " - " & Ada.Exceptions.Exception_Name (E) & " message: " & Ada.Exceptions.Exception_Message (E));
            Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
         end if;
      end Logger;

      procedure Logger (S : Ada.Strings.Unbounded.Unbounded_String; E : Ada.Exceptions.Exception_Occurrence) is
         Trace        : GNAT.Traceback.Tracebacks_Array (1 .. 100);
         Length       : Natural;
      begin
         GNAT.Traceback.Call_Chain (Trace, Length);

         if Log_To_Syslog then
            GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, Ada.Strings.Unbounded.To_String (S) & " - " & Ada.Exceptions.Exception_Name (E) & " message: " & Ada.Exceptions.Exception_Message (E));
            GNATCOLL.Traces.Syslog.Syslog (GNATCOLL.Traces.Syslog.Daemon, GNATCOLL.Traces.Syslog.Critical, GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
         end if;

         if Log_To_stdout then
            Ada.Strings.Unbounded.Text_IO.Put_Line (S & " - " & Ada.Exceptions.Exception_Name (E) & " message: " & Ada.Exceptions.Exception_Message (E));
            Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
         end if;
      end Logger;
   end Secure;
end Logger;
