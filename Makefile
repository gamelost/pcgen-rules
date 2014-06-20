VERIFY="./verify.sh"

all:
	cabal build

.PHONY: all

test-languages:
	$(VERIFY) LANGUAGE "languages"

test-shieldprof:
	$(VERIFY) SHIELDPROF "(prof_shield|profs_shield|shieldprof)"

test-armorprof:
	$(VERIFY) ARMORPROF "(profs_armor|armor_prof|profsarmor|armorprof)"

test-weaponprof:
	$(VERIFY) WEAPONPROF "(profs_weapon|weaponprof|prof_weapon|weapon_prof)"

test-skill:
	$(VERIFY) SKILL "skill"

clean:
	cabal clean
