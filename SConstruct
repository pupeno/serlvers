# -*- coding: utf-8 -*-
# Copyright (C) 2005 José Pablo Ezequiel "Pupeno" Fernández Silva
#
# This file is part of serlvers.
#
# Serlvers is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.
# Serlvers is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with serlvers; if not, write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA

env = Environment(tools = ["default", "erlang"])

# Configuration.
configFile = ".Serlvers.conf"
opts = Options(configFile)
opts.Add(PathOption("PREFIX", "Prefix directory (where Erlang is installed)", "/usr/local/lib/erlang/"))
opts.Update(env)
opts.Save(configFile, env)

# Help.
Help(opts.GenerateHelpText(env))

sources = ["src/launcher.erl",
           "src/gen_echo.erl",
           "src/gen_chargen.erl",
           "src/gen_daytime.erl",
           "src/gen_time.erl",
           "src/gen_dns.erl"]

# Compile the sources to beams.
beams = env.Erlang(sources)

# Install directories.
installDir = "$PREFIX/lib/serlvers-0.1.0/"

# chicken.py, no build needed.
env.Install(installDir + "ebin/", beams)
env.Install(installDir + "ebin/", "ebin/serlvers.app")
env.Install(installDir + "src/", sources)

# Alias for installing.
env.Alias("install", installDir)

# Documentation
env.EDoc("doc/index.html", sources)
