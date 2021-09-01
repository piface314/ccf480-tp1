TARGET=ccf480-tp1
OUT=result.csv
PLOTDIR=figs

all:
	ghc -o $(TARGET) -O src/*.hs -odir obj/ -hidir obj/

clean:
	rm -rf $(TARGET) obj/*.o obj/*.hi

run:
	./$(TARGET) $(OUT) 30
	mkdir -p $(PLOTDIR)
	python3 report.py $(OUT) $(PLOTDIR)
