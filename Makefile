version:
	echo -n '' > inst/VERSION && echo -n `git describe --tags $(git rev-list --tags --max-count=2) | cut -d'-' -f1` >> inst/VERSION && echo -n " | " >> inst/VERSION && echo `git log -1 --pretty=format:"%cd (%h)" --date=local` >> inst/VERSION
