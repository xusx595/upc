#include <stdio.h>
#include <string.h>

char file_name1[2048], file_name2[2048];
int line1;
int line2;
const char *template = "%s %d\n";


void read_until_same(FILE *fp, char *name, char *where) {

  int lineno;
  while(!feof(fp)) {
    fscanf(fp, template, where, &lineno);
    if (strcmp(name, where) == 0)
      break;
  }
}

void read_until_different(FILE *fp, char *name, char *where) {

  int lineno;
  while(!feof(fp)) {
    fscanf(fp, template, where, &lineno);
    if (strcmp(name, where) != 0)
      break;
  }
}


int  main(int argc, char *argv[])
{
  int lineno;
  FILE *infile, *outfile;
  
  if(argc != 4) {
    printf("Usage: %s infile outfile srcfile\n", argv[0]);
    exit(1);
  }
  
  

  infile = fopen(argv[1], "r+");
  if(!infile) {
    printf("Can't open %s\n", argv[1]);
    exit(1);
  }

  outfile = fopen(argv[2], "w+");
  if(!outfile) {
    printf("Can't open %s\n", argv[2]);
    exit(1);
  }
  
  while(!feof(infile)) {
    read_until_different(infile, argv[3], file_name2);
    fprintf(outfile, "%s\n", file_name2);
    read_until_same(infile, argv[3], file_name2);
  }
  return 0;
}

