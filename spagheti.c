
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <GL/gl.h>
#include <GL/glut.h>
#include <math.h>

#define X_AXIS  0
#define Y_AXIS  1
#define Z_AXIS  2

float h = 250;
float w = 250;
int a, b, c, q;
int zoom;

/* simulation box dimensions and monomer length, read from sparg.ini */
float dimx = 15.0f;
float dimy = 15.0f;
float dimz = 100.0f;
float comprimento = 0.625f;

/* monomer data — allocated dynamically after reading the count */
float *posMono[6];
int   *numMono;
FILE  *ioptr;
int    j, i;
int    Numero;

typedef float vec3_t[3];
enum {
    UP = 1,
    DOWN,
};

/* mouse state */
int oldX = -13;
int oldY = -13;
int oldZ = -13;
int mState  = UP;
int mState1 = UP;
int axisRot = X_AXIS;
float  rotate = 0.0f;
vec3_t axis = {1.0, 0.0, 0.0};
vec3_t gRot = {0, 0, 0};
int aniOn = 0;


/* ---------- read DIMENSOES and COMPRIMENTO from sparg.ini ---------- */
void leitura_ini(void)
{
    FILE *f;
    char  line[256];
    char  key[4];

    f = fopen("sparg.ini", "r");
    if (f == NULL) {
        fprintf(stderr,
            "Aviso: nao consigo abrir sparg.ini, usando dimensoes por defeito.\n");
        return;
    }

    while (fgets(line, sizeof(line), f)) {
        key[0] = (char)tolower((unsigned char)line[0]);
        key[1] = (char)tolower((unsigned char)line[1]);
        key[2] = (char)tolower((unsigned char)line[2]);
        key[3] = '\0';

        if (strcmp(key, "dim") == 0) {
            if (fgets(line, sizeof(line), f))
                sscanf(line, "%f %f %f", &dimx, &dimy, &dimz);
        } else if (strcmp(key, "com") == 0) {
            if (fgets(line, sizeof(line), f))
                sscanf(line, "%f", &comprimento);
        } else if (strcmp(key, "end") == 0) {
            break;
        }
    }

    fclose(f);
}


/* ---------- read monomeros.txt ---------- */
void leitura(void)
{
    int k;

    ioptr = fopen("monomeros.txt", "r");
    if (ioptr == NULL) {
        fprintf(stderr, "Nao consigo abrir o ficheiro monomeros.txt\n");
        exit(1);
    }

    if (fscanf(ioptr, "%i", &Numero) != 1) {
        fprintf(stderr, "Erro ao ler numero de monomeros.\n");
        exit(1);
    }

    /* allocate arrays now that Numero is known */
    for (k = 0; k < 6; k++) {
        posMono[k] = malloc(Numero * sizeof(float));
        if (posMono[k] == NULL) {
            fprintf(stderr, "Erro ao alocar memoria para posMono[%d].\n", k);
            exit(1);
        }
    }
    numMono = malloc(Numero * sizeof(int));
    if (numMono == NULL) {
        fprintf(stderr, "Erro ao alocar memoria para numMono.\n");
        exit(1);
    }

    for (j = 0; j < Numero; j++) {
        if (fscanf(ioptr, "%i %f %f %f %f %f %f",
                   &numMono[j],
                   &posMono[0][j], &posMono[1][j], &posMono[2][j],
                   &posMono[3][j], &posMono[4][j], &posMono[5][j]) != 7) {
            fprintf(stderr, "Erro ao ler monomero %d.\n", j);
            exit(1);
        }
    }

    fclose(ioptr);
}


/* ---------- keyboard ---------- */
void glutKeyboard(unsigned char key, int x, int y)
{
    switch (key) {
        case 'q':
        case 'Q':
            exit(1);
        break;
        case '-':
            a += 1;
        break;
        case '+':
            a -= 1;
        break;
        case 'z':
            zoom += 10;
        break;
        case 'x':
            zoom -= 10;
        break;
        default:
            return;
    }
    glutPostRedisplay();
}

void teclado(int key, int x, int y)
{
    switch (key) {
        case GLUT_KEY_UP:
            b -= 1;
        break;
        case GLUT_KEY_DOWN:
            b += 1;
        break;
        case GLUT_KEY_LEFT:
            c += 1;
        break;
        case GLUT_KEY_RIGHT:
            c -= 1;
        break;
        default:
            return;
    }
    glutPostRedisplay();
}


/* ---------- drawing ---------- */
void draw(void)
{
    glBegin(GL_LINES);
    for (q = 0; q < Numero; q++) {
        glColor3d(0.2 * cos(posMono[3][q]) * cos(posMono[3][q]),
                  0.5 * cos(posMono[3][q]) * cos(posMono[3][q]),
                        cos(posMono[3][q]) * cos(posMono[3][q]));
        glVertex3d(posMono[0][q],
                   posMono[1][q],
                   posMono[2][q]);
        glVertex3d(posMono[0][q] + comprimento * sin(posMono[3][q]) * cos(posMono[5][q]),
                   posMono[1][q] + comprimento * sin(posMono[3][q]) * sin(posMono[5][q]),
                   posMono[2][q] + comprimento * cos(posMono[3][q]));
    }
    glEnd();
    glFlush();
}

void drawrect(void)
{
    glPushMatrix();

    /* bottom face */
    glBegin(GL_LINE_STRIP);
    glColor3f(1.0, 1.0, 1.0);
    glVertex3f(0.0,  0.0,  0.0);
    glVertex3f(0.0,  dimy, 0.0);
    glVertex3f(dimx, dimy, 0.0);
    glVertex3f(dimx, 0.0,  0.0);
    glVertex3f(0.0,  0.0,  0.0);
    glEnd();

    /* top face */
    glBegin(GL_LINE_STRIP);
    glColor3f(1.0, 1.0, 1.0);
    glVertex3f(0.0,  0.0,  dimz);
    glVertex3f(0.0,  dimy, dimz);
    glVertex3f(dimx, dimy, dimz);
    glVertex3f(dimx, 0.0,  dimz);
    glVertex3f(0.0,  0.0,  dimz);
    glEnd();

    /* front face */
    glBegin(GL_LINE_STRIP);
    glColor3f(1.0, 1.0, 1.0);
    glVertex3f(0.0,  0.0, 0.0);
    glVertex3f(0.0,  0.0, dimz);
    glVertex3f(dimx, 0.0, dimz);
    glVertex3f(dimx, 0.0, 0.0);
    glVertex3f(0.0,  0.0, 0.0);
    glEnd();

    /* back face */
    glBegin(GL_LINE_STRIP);
    glColor3f(1.0, 1.0, 1.0);
    glVertex3f(0.0,  dimy, 0.0);
    glVertex3f(0.0,  dimy, dimz);
    glVertex3f(dimx, dimy, dimz);
    glVertex3f(dimx, dimy, 0.0);
    glVertex3f(0.0,  dimy, 0.0);
    glEnd();

    glFlush();
    glPopMatrix();
}


/* ---------- display / reshape ---------- */
void Display(void)
{
    glClearColor(0.5, 0.5, 0.5, 0.0);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();

    glTranslatef(0, 0, -a);
    glTranslatef(0, -b, 0);
    glTranslatef(-c, 0, 0);
    glTranslatef(0, 0, -(dimz + 10.0f));

    glRotatef(gRot[0], 1.0, 0.0, 0.0);
    glRotatef(gRot[1], 0.0, 1.0, 0.0);

    glPushMatrix();
    draw();
    drawrect();
    glPopMatrix();

    glutSwapBuffers();
}

void Resize(int width, int height)
{
    if (height == 0)
        height = 1;

    glViewport(0, 0, width, height);

    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    gluPerspective(45, (float)width / height, 1, 100000);

    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
}


/* ---------- mouse ---------- */
void clamp(vec3_t v)
{
    int ii;
    for (ii = 0; ii < 3; ii++)
        if (v[ii] >= 360 || v[ii] < -360)
            v[ii] = 0;
}

void glutMotion(int x, int y)
{
    if (mState == DOWN) {
        gRot[0] -= ((oldY - y) * 180.0f) / 100.0f;
        gRot[1] -= ((oldX - x) * 180.0f) / 100.0f;
        clamp(gRot);
        glutPostRedisplay();
    }
    oldX = x;
    oldY = y;
}

void glutMouse(int button, int state, int x, int y)
{
    if (state == GLUT_DOWN) {
        if (button == GLUT_LEFT_BUTTON) {
            mState = DOWN;
            oldX = x;
            oldY = y;
        }
    } else if (state == GLUT_UP) {
        mState = UP;
    }
}


/* ---------- init ---------- */
void glInit(void)
{
    glDepthFunc(GL_LEQUAL);
    leitura_ini();
    leitura();
}


/* ---------- main ---------- */
int main(int argcp, char **argv)
{
    glutInit(&argcp, argv);
    glutInitDisplayMode(GLUT_RGB | GLUT_DOUBLE | GLUT_DEPTH);
    glutInitWindowSize((int)w, (int)h);
    glutCreateWindow("Massa");
    glutReshapeFunc(Resize);
    glutDisplayFunc(Display);
    glutKeyboardFunc(glutKeyboard);
    glutSpecialFunc(teclado);
    glutMouseFunc(glutMouse);
    glutMotionFunc(glutMotion);
    glInit();
    glutMainLoop();
    return 0;
}
