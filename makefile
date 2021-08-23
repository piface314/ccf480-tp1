TARGET=ccf480-tp1

all:
	ghc -o $(TARGET) -O src/*.hs -odir obj/ -hidir obj/

clean:
	rm -rf $(TARGET) obj/*.o obj/*.hi

run:
	./$(TARGET)