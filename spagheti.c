
#include <stdio.h>
#include <string.h>
#include <GL/gl.h>
#include <GL/glut.h>
#include <math.h>
#define GL_PI                    3.1415f
#define l                             0.625
#define X_AXIS                  0
#define Y_AXIS                  1
#define Z_AXIS                  2
float h=250;
float w=250;
int a,b,c,q;
int zoom;
float posMono[6][100000];
int numMono[100000];
FILE *ioptr;
int j,i;
typedef float vec3_t[3];
enum {
    UP = 1,
    DOWN,
};


 /* posições do rato*/
int oldX = -13;
int oldY = -13;
int oldZ = -13;
/* estado do rato  UP ou DOWN */
int mState = UP;
int mState1 = UP;
int axisRot = X_AXIS;
float rotate = 0.0f;
vec3_t axis = {1.0, 0.0, 0.0};
vec3_t gRot = {0,0,0};
int aniOn = 0;
int Numero;


void leitura (void)
{


        ioptr = fopen("monomeros.txt","r");

        if (ioptr == (FILE *)NULL)
        {
              printf("Nao consigo abrir o ficheiro\n");
              exit(1);
        }
	
	fscanf(ioptr,"%i",&Numero);
           
   		for (j=0;j<Numero;j++)
                {

        fscanf(ioptr, "%i %f %f %f %f %f %f",&numMono[j],  &posMono[0][j], &posMono[1][j], &posMono[2][j], &posMono[3][j], &posMono[4][j], &posMono[5][j]);


             }
}



void trans(void)
{

        a +=1.0;


}

void trans_(void)
{

        a -= 1.0;

}

void transy(void)
{

        b +=1.0;

}

void transy_(void)
{

        b -=1.0;

}

void transx(void)
{

        c +=1.0;

}

void transx_(void)
{

        c -=1.0;

}

void zoon(void)
{

        zoom +=10.0;

}
void zoon_(void)
{

        zoom -=10.0;

}


void glutKeyboard (unsigned char key, int x, int y)
{
    switch (key)
    {
        case 'q':
        case 'Q':
            exit (1);
        break;

        break;
        case '-':
            trans();
        break;

        case '+':
            trans_();
        break;

        case 'z':
            zoon();
        break;

        case 'x':
            zoon_();
        break;


        default:
        return;
     }
    glutPostRedisplay ();
}

void teclado(int key,int x,int y)
{
        switch(key)
        {

        case GLUT_KEY_UP :
            transy_();
        break;
        case GLUT_KEY_DOWN:
            transy();
        break;
        case GLUT_KEY_LEFT:
            transx();
        break;
        case GLUT_KEY_RIGHT:
             transx_();
        break;

        default:
        return;
     }
    glutPostRedisplay ();
}
void draw(void)
{
        glBegin(GL_LINES);
              for (q=0;q<Numero;q++)
               {
//        printf("%f %f\n",posMono[3][q],cos(posMono[3][q])*cos(posMono[3][q])); 

	glColor3d (0.2*cos(posMono[3][q])*cos(posMono[3][q]),0.5*cos(posMono[3][q])*cos(posMono[3][q]),cos(posMono[3][q])*cos(posMono[3][q]));
	glVertex3d(posMono[0][q],posMono[1][q],posMono[2][q]);
        glVertex3d(posMono[0][q]+l*sin(posMono[3][q])*cos(posMono[5][q]),posMono[1][q]+l*sin(posMono[3][q])*sin(posMono[5][q]),posMono[2][q]+l*cos(posMono[3][q]));
		 
               }
        glEnd();
         glFlush();
}

void drawrect(void)
{
        glPushMatrix();
        glBegin(GL_LINE_STRIP);
        glColor3f(1.0,1.0,1.0);
        glVertex3f(0.0,0.0,0.0);
        glVertex3f(0.0,15.0,0.0);
        glVertex3f(15.0,15.0,0.0);
        glVertex3f(15.0,0.0,0.0);
        glVertex3f(0.0,0.0,0.0);
        glEnd();
        glFlush();
        glBegin(GL_LINE_STRIP);
        glColor3f(1.0,1.0,1.0);
        glVertex3f(0.0,0.0,100.0);
        glVertex3f(0.0,15.0,100.0);
        glVertex3f(15.0,15.0,100.0);
        glVertex3f(15.0,0.0,100.0);
        glVertex3f(0.0,0.0,100.0);
        glEnd();
        glFlush();
        glBegin(GL_LINE_STRIP);
        glColor3f(1.0,1.0,1.0);
        glVertex3f(0.0,0.0,0.0);
        glVertex3f(0.0,0.0,100.0);
        glVertex3f(15.0,0.0,100.0);
        glVertex3f(15.0,0.0,0.0);
        glVertex3f(0.0,0.0,0.0);
        glEnd();
        glFlush();
        glBegin(GL_LINE_STRIP);
                glColor3f(1.0,1.0,1.0);
                glVertex3f(0.0,15.0,0.0);
                glVertex3f(0.0,15.0,100.0);
                glVertex3f(15.0,15.0,100.0);
                glVertex3f(15.0,15.0,0.0);
                glVertex3f(0.0,15.0,0.0);
        glEnd();
        glFlush();
        glPopMatrix();

}


void Display (void)
{
      float  x,y,z,xa,ya,za,alfa,beta,gama;
       int f = 10;
      glClearColor(0.5,0.5,0.5,0.0);
      glClear (GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

         glMatrixMode(GL_MODELVIEW);
          glLoadIdentity();

          glTranslatef (0, 0, -a);
          glTranslatef (0,-b,0);
          glTranslatef (-c,0,0);
          glTranslatef (0,0,-110);

          glRotatef (gRot[0], 1.0, 0.0, 0.0);
          glRotatef (gRot[1], 0.0, 1.0, 0.0);

         glPushMatrix();
          draw();
         drawrect();
         glPopMatrix();
         glutSwapBuffers();
}

 void Resize (int w, int h)
{
    if (h ==0)
           h = 1;

    glViewport (0, 0, w, h);

    glMatrixMode (GL_PROJECTION);
    glLoadIdentity ();

    gluPerspective (45, w / h, 1, 100000);

       glMatrixMode(GL_MODELVIEW);
      glLoadIdentity();
} 

/*void Resize(GLsizei w,GLsizei h)
{
        //dimensão do volume
        GLfloat nRange = 50.0f;
        //previne a divisão por zero
        if ( h==0 )
                h = 1;

        //define o viewport para as dimensões da janela
        glViewport(0,0,w,h);

        //renicializa o sistema de coordenadas
       glMatrixMode(GL_PROJECTION);
        glLoadIdentity();

        //volume de clipping  (left,right,bottom,top,near,far)
        if (w<=h)
                glOrtho (-15,15,-15*h/w,15*h/w, -160,160);
        else
                glOrtho (-15*w/h,15*w/h,-15,15,-160,160);
      glMatrixMode(GL_MODELVIEW);
      glLoadIdentity();
} */

void clamp (vec3_t v)
{
    int i;

    for (i = 0; i < 3; i ++)
        if (v[i] >= 360 || v[i] < -360)
            v[i] = 0;
}



void glutMotion(int x, int y )
{
    if (mState == DOWN)
    {
        gRot[0] -= ((oldY - y) * 180.0f) / 100.0f;
        gRot[1] -= ((oldX - x) * 180.0f) / 100.0f;

        clamp (gRot);
        glutPostRedisplay ();
    }


    oldX = x;
    oldY = y;


}

/***********

RATO

**********/

void glutMouse(int button, int state, int x, int y)
{
    if(state == GLUT_DOWN)
    {
        switch(button)
        {
            case GLUT_LEFT_BUTTON:


                mState = DOWN;
                oldX = x;
                oldY = y;

            break;

        }
    } else if (state == GLUT_UP)
        mState = UP;

}

  void glInit (void)
{
  glDepthFunc(GL_LEQUAL);
    //glEnable (GL_DEPTH_TEST);
    
     leitura();
}


 int  main(int argcp, char **argv)
{
        draw();
        drawrect();
        glutInit(&argcp,argv);
        glutInitDisplayMode (GLUT_RGB | GLUT_DOUBLE | GLUT_DEPTH  );
        glutInitWindowSize (w,h);
        glutCreateWindow ("Massa ");
        glutReshapeFunc(Resize);
        glutDisplayFunc(Display);
        glutKeyboardFunc (glutKeyboard);
        glutSpecialFunc(teclado);
        glutMouseFunc (glutMouse);
        glutMotionFunc (glutMotion);
        glInit ();
        glutMainLoop();
}
