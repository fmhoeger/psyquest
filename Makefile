version:
	rm VERSION && echo -n `git describe --tags $(git rev-list --tags --max-count=1)` > VERSION && echo -n " | " >> VERSION && echo `git log -1 --pretty=format:"%cd (%h)" --date=local` >> VERSION
