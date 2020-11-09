TAG?=temp

tag:
	git tag -a $(TAG) -m "Version $(TAG)"
	git push origin --tags

build:
	mkdir -p releases
	mkdir -p releases/$(shell basename $(CURDIR))_$(TAG)
	pandoc README.md -o releases/$(shell basename $(CURDIR))_$(TAG)/README.pdf
	rsync -av --progress . releases/$(shell basename $(CURDIR))_$(TAG) --exclude releases --exclude .git --exclude README.md --exclude .Rproj.user

clean:
	rm -rf output
	rm README.pdf
