PROJECT = upnp

DEPS = hackney lager cowboy
dep_hackney = git://github.com/benoitc/hackney.git master
dep_lager   = git://github.com/basho/lager.git master
dep_cowboy  = git://github.com/refuge/cowboy.git master

include erlang.mk


