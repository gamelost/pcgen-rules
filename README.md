
pcgen-rules
===========

pcgen-rules extracts and processes the rules and data from [PCGen](http://pcgen.sourceforge.net/01_overview.php) in order to make them more accessible to other services or programs.

You will need the PCGen data files in a separate directory. Edit the `Makefile` accordingly if you use a directory other than `data/`.

Currently the code just parses `.pcc` and `.lst` files; this is still a work in progress. See [todo.org](todo.org) for a more detailed listing of what needs to be done. (I have begun the process of cleaning up the PCGen data as certain parts were too messy to deal with. Ping me if you want an updated data/ listing; I plan to push this listing upstream eventually.)

`.lst` file parsing status:

- [x] LANGUAGE (76/76)
- [x] SHIELDPROF (28/28)
- [x] WEAPONPROF (130/130)
- [x] ARMORPROF (50/50)
- [x] SKILL (138/139)
- [x] COMPANIONMOD (block-based) (50/51)
- [x] DEITY (block-based) (62/62)
- [x] DOMAIN (48/49)
- [x] EQUIPMOD (113/115)
- [x] EQUIPMENT (695/709)
- [x] SPELL (205/209)
- [ ] FEAT (0/347)
- [ ] RACE (0/281)
- [ ] KIT (block-based) (0/284)
- [ ] TEMPLATE (0/192)
- [ ] CLASS (block-based) (0/304)
- [ ] ABILITY (0/424)
- [ ] ABILITYCATEGORY (0/165)
