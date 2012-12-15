#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <GL/glut.h>
#include "glf.h"

/*
   Test 3dtext from the GLF library
   Paul Bourke, August 2000
*/

typedef struct {
   double x,y,z;
} XYZ;
typedef struct {
   double r,g,b;
} COLOUR;
typedef struct {
   unsigned char r,g,b,a;
} PIXELA;
typedef struct {
   XYZ vp;              /* View position           */
   XYZ vd;              /* View direction vector   */
   XYZ vu;              /* View up direction       */
   XYZ pr;              /* Point to rotate about   */
   double focallength;  /* Focal Length along vd   */
   double aperture;     /* Camera aperture         */
   double eyesep;       /* Eye separation          */
} CAMERA;

void Display(void);
void CreateEnvironment(void);
void MakeGeometry(void);
void HandleKeyboard(unsigned char key,int x, int y);
void HandleSpecialKeyboard(int key,int x, int y);
void HandleMouse(int,int,int,int);
void HandleMainMenu(int);
void HandleFontMenu(int);
void HandleStyleMenu(int);
void HandleMenu(int);
void HandleVisibility(int vis);
void HandleReshape(int,int);
void HandleMouseMotion(int,int);
void HandlePassiveMotion(int,int);
void HandleIdle(void);
void GiveUsage(char *);
void RotateCamera(int,int,int);
void TranslateCamera(int,int);
void FlyCamera(int);
void CameraHome(int);
void Normalise(XYZ *);
XYZ  CalcNormal(XYZ,XYZ,XYZ);
int  WindowDump(int,int,int);

#define ABS(x) (x < 0 ? -(x) : (x))
#define MIN(x,y) (x < y ? x : y)
#define MAX(x,y) (x > y ? x : y)
#define TRUE  1
#define FALSE 0
#define ESC 27
#define PI 3.141592653589793238462643
#define DTOR            0.0174532925
#define RTOD            57.2957795
#define CROSSPROD(p1,p2,p3) \
   p3.x = p1.y*p2.z - p1.z*p2.y; \
   p3.y = p1.z*p2.x - p1.x*p2.z; \
   p3.z = p1.x*p2.y - p1.y*p2.x

/* Flags */
int fullscreen = FALSE;
int stereo = FALSE;
int showconstruct = FALSE;
int windowdump = FALSE;
int record = FALSE;
int debug = FALSE;

int screenwidth=800,screenheight=600;
int currentbutton = -1;
double rotatespeed = 1;
double dtheta = 1;
CAMERA camera;
XYZ origin = {0.0,0.0,0.0};

int whichfont,thefont[11];
int whatstyle = 1;
#define THEMSG "This is the text being displayed"

int main(int argc,char **argv)
{
   int i;
   int mainmenu,fontmenu,stylemenu;

   /* Parse the command line arguments */
   for (i=1;i<argc;i++) {
      if (strstr(argv[i],"-h") != NULL) 
         GiveUsage(argv[0]);
      if (strstr(argv[i],"-f") != NULL)
         fullscreen = TRUE;
      if (strstr(argv[i],"-s") != NULL)
         stereo = TRUE;
      if (strstr(argv[i],"-d") != NULL)
         debug = TRUE;
      if (strstr(argv[i],"-c") != NULL)
         showconstruct = TRUE;
   }

   /* Set things up and go */
   glutInit(&argc,argv);
   if (!stereo)
      glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH);
   else
      glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH | GLUT_STEREO);

   glutCreateWindow("3d text test");
   glutReshapeWindow(200,180);
   if (fullscreen)
      glutFullScreen();
   glutDisplayFunc(Display);
   glutReshapeFunc(HandleReshape);
   glutVisibilityFunc(HandleVisibility);
   glutKeyboardFunc(HandleKeyboard);
   glutSpecialFunc(HandleSpecialKeyboard);
   glutMouseFunc(HandleMouse);
   glutMotionFunc(HandleMouseMotion);
   glutSetCursor(GLUT_CURSOR_NONE);
   CreateEnvironment();
   CameraHome(0);

   /* Initialise the library */
   glfInit();

   /* Install all the fonts */
   whichfont = 0;
   thefont[ 0] = glfLoadFont("fonts/penta1.glf");
   thefont[ 1] = glfLoadFont("fonts/courier1.glf");
   thefont[ 2] = glfLoadFont("fonts/crystal1.glf");
   thefont[ 3] = glfLoadFont("fonts/techno0.glf");
   thefont[ 4] = glfLoadFont("fonts/techno1.glf");
   thefont[ 5] = glfLoadFont("fonts/times_new1.glf");
   thefont[ 6] = glfLoadFont("fonts/broadway1.glf");
   thefont[ 7] = glfLoadFont("fonts/cricket1.glf");
   thefont[ 8] = glfLoadFont("fonts/garamond1.glf");
   thefont[ 9] = glfLoadFont("fonts/gothic1.glf");
   thefont[10] = glfLoadFont("fonts/arial1.glf");

   /* Set up the font menu */
   fontmenu = glutCreateMenu(HandleFontMenu);
   glutAddMenuEntry("penta1",1);
   glutAddMenuEntry("courier1",2);
   glutAddMenuEntry("crystal1",3);
   glutAddMenuEntry("techno0",4);
   glutAddMenuEntry("techno1",5);
   glutAddMenuEntry("times_new1",6);
   glutAddMenuEntry("broadway1",7);
   glutAddMenuEntry("cricket1",8);
   glutAddMenuEntry("garamond1",9);
   glutAddMenuEntry("gothic1",10);
   glutAddMenuEntry("arial1.glf",11);

   /* Set up the style menu */
   stylemenu = glutCreateMenu(HandleStyleMenu);
   glutAddMenuEntry("glfDrawWiredString",1);
   glutAddMenuEntry("glfDrawSolidString",2);
   glutAddMenuEntry("glfDraw3DWiredString",3);
   glutAddMenuEntry("glfDraw3DSolidString",4);

   /* Set up the main menu */
   mainmenu = glutCreateMenu(HandleMainMenu);
   glutAddSubMenu("Font",fontmenu);
   glutAddSubMenu("Style",stylemenu);
   glutAddMenuEntry("Quit",9);
   glutAttachMenu(GLUT_RIGHT_BUTTON);

   /* Ready to go! */
   glutMainLoop();
   return(0);
}

/*
   This is where global OpenGL/GLUT settings are made, 
   that is, things that will not change in time 
*/
void CreateEnvironment(void)
{
   GLfloat shininess[1] = {40.0};
   GLfloat position1[4] = {0.0,10.0,10.0,0.0};
   GLfloat position2[4] = {10.0,-4.0,-4.0,0.0};
   GLfloat ambient[4]  = {0.4,0.4,0.4,1.0};
   GLfloat white[4]  = {1.0,1.0,1.0,1.0};
   GLfloat black[4] = {0.0,0.0,0.0,1.0};

   glEnable(GL_DEPTH_TEST);
   glDisable(GL_LINE_SMOOTH);
   glDisable(GL_POINT_SMOOTH);
   glEnable(GL_POLYGON_SMOOTH); 
   glShadeModel(GL_SMOOTH);    
   glDisable(GL_DITHER);
   glDisable(GL_CULL_FACE);

   glLineWidth(1.0);
   glPointSize(1.0);

   glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
   glFrontFace(GL_CW);
   glClearColor(0.0,0.0,0.0,0.0);
   glColorMaterial(GL_FRONT_AND_BACK,GL_AMBIENT_AND_DIFFUSE);
   glEnable(GL_COLOR_MATERIAL);
   glPixelStorei(GL_UNPACK_ALIGNMENT,1);

   glShadeModel(GL_SMOOTH);
   glMaterialfv(GL_FRONT_AND_BACK,GL_SPECULAR,white);
   glMaterialfv(GL_FRONT_AND_BACK,GL_SHININESS,shininess);

   /* Turn off all the lights */
   glDisable(GL_LIGHT0);
   glDisable(GL_LIGHT1);
   glDisable(GL_LIGHT2);
   glDisable(GL_LIGHT3);
   glDisable(GL_LIGHT4);
   glDisable(GL_LIGHT5);
   glDisable(GL_LIGHT6);
   glDisable(GL_LIGHT7);
   glLightModeli(GL_LIGHT_MODEL_LOCAL_VIEWER,GL_TRUE);
   glLightModeli(GL_LIGHT_MODEL_TWO_SIDE,GL_FALSE);

   /* Turn on the appropriate lights */
   glLightModelfv(GL_LIGHT_MODEL_AMBIENT,ambient);
   glLightfv(GL_LIGHT0,GL_POSITION,position1);
   glLightfv(GL_LIGHT0,GL_AMBIENT,black);
   glLightfv(GL_LIGHT0,GL_DIFFUSE,white);
   glLightfv(GL_LIGHT0,GL_SPECULAR,white);
   glEnable(GL_LIGHT0);
   glLightfv(GL_LIGHT1,GL_POSITION,position2);
   glLightfv(GL_LIGHT1,GL_AMBIENT,black);
   glLightfv(GL_LIGHT1,GL_DIFFUSE,white);
   glLightfv(GL_LIGHT1,GL_SPECULAR,white);
   glEnable(GL_LIGHT1);
}

/*
   This is the basic display callback routine
   It creates the geometry, lighting, and viewing position
   In this case it rotates the camera around the scene
*/
void Display(void)
{
   XYZ right,focus;

   /* Clear the buffers */
   glDrawBuffer(GL_BACK_LEFT);
   glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
   if (stereo) {
      glDrawBuffer(GL_BACK_RIGHT);
      glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
   }

   /* Determine the focal point */
   Normalise(&camera.vd);
   focus.x = camera.vp.x + camera.focallength * camera.vd.x;
   focus.y = camera.vp.y + camera.focallength * camera.vd.y;
   focus.z = camera.vp.z + camera.focallength * camera.vd.z;

   glMatrixMode(GL_PROJECTION);
   glLoadIdentity();
   gluPerspective(camera.aperture,
      screenwidth/(double)screenheight,0.1,10000.0);

   if (stereo) {

      /* Derive the two eye positions */
      CROSSPROD(camera.vd,camera.vu,right);
      Normalise(&right);
      right.x *= camera.eyesep / 2.0;
      right.y *= camera.eyesep / 2.0;
      right.z *= camera.eyesep / 2.0;

      glMatrixMode(GL_MODELVIEW);
      glDrawBuffer(GL_BACK_RIGHT);
      glLoadIdentity();
      gluLookAt(camera.vp.x + right.x,
                camera.vp.y + right.y,
                camera.vp.z + right.z,
                focus.x,focus.y,focus.z,
                camera.vu.x,camera.vu.y,camera.vu.z);
      MakeGeometry();

      glMatrixMode(GL_MODELVIEW);
      glDrawBuffer(GL_BACK_LEFT);
      glLoadIdentity();
      gluLookAt(camera.vp.x - right.x,
                camera.vp.y - right.y,
                camera.vp.z - right.z,
                focus.x,focus.y,focus.z,
                camera.vu.x,camera.vu.y,camera.vu.z);
      MakeGeometry();
   } else {
      glMatrixMode(GL_MODELVIEW);
      glDrawBuffer(GL_BACK_LEFT);
      glLoadIdentity();
      gluLookAt(camera.vp.x,
                camera.vp.y,
                camera.vp.z,
                focus.x,focus.y,focus.z,
                camera.vu.x,camera.vu.y,camera.vu.z);
      MakeGeometry();
   }

   /* glFlush(); This isn't necessary for double buffers */
   glutSwapBuffers();

   if (record || windowdump) {
      WindowDump(screenwidth,screenheight,stereo);
      windowdump = FALSE;
   }
}

/*
   Create the geometry
*/
void MakeGeometry(void)
{
   float xmin,ymin,xmax,ymax;

   glfSetCurrentFont(thefont[whichfont]);
   glfGetStringBounds(THEMSG,&xmin,&ymin,&xmax,&ymax);

   glPushMatrix();
   glTranslatef(-(xmin+xmax)/2,0.0,0.0);
   glScalef(1.0,1.0,1.0);
   glColor3f(1.0,0.2,0.3);
   switch (whatstyle) {
   case 1:
      glDisable(GL_LIGHTING);
        glfDrawWiredString(THEMSG);
      break;
   case 2:
      glEnable(GL_LIGHTING);
      glfDrawSolidString(THEMSG);
      break;
   case 3:
      glfSetSymbolDepth(0.5);
      glDisable(GL_LIGHTING);
      glfDraw3DWiredString(THEMSG);
      break;
   case 4:
      glEnable(GL_LIGHTING);
      glfSetSymbolDepth(1.0);
      glfDraw3DSolidString(THEMSG);
      break;
   }
   glPopMatrix();
}

/*
   Deal with plain key strokes
*/
void HandleKeyboard(unsigned char key,int x, int y)
{
   switch (key) {
   case ESC:                            /* Quit */
   case 'Q':
   case 'q': 
      exit(0); 
      break;
   case 'c':                           /* Toggle constructs */
   case 'C':
      showconstruct = !showconstruct;
      break;
   case 'h':                           /* Go home     */
   case 'H':
      CameraHome(0);
      break;
   case '[':                           /* Roll anti clockwise */
      RotateCamera(0,0,-1);
      break;
   case ']':                           /* Roll clockwise */
      RotateCamera(0,0,1);
      break;
   case 'i':                           /* Translate camera up */
   case 'I':
      TranslateCamera(0,1);
      break;
   case 'k':                           /* Translate camera down */
   case 'K':
      TranslateCamera(0,-1);
      break;
   case 'j':                           /* Translate camera left */
   case 'J':
      TranslateCamera(-1,0);
      break;
   case 'l':                           /* Translate camera right */
   case 'L':
      TranslateCamera(1,0);
      break;
   case '+':
   case '=':
      FlyCamera(1);
      break;
   case '-':
   case '_':
      FlyCamera(-1);   
      break;
   case 'w':                           /* Write the image to disk */
   case 'W':
      windowdump = !windowdump;
      break;
   case 'r':
   case 'R':
      record = !record;
      break;
   }
}

/*
   Deal with special key strokes
*/
void HandleSpecialKeyboard(int key,int x, int y)
{
   switch (key) {
   case GLUT_KEY_LEFT:
      RotateCamera(-1,0,0);
      break;
   case GLUT_KEY_RIGHT:
      RotateCamera(1,0,0);
      break;
   case GLUT_KEY_UP:
      RotateCamera(0,1,0);
      break;
   case GLUT_KEY_DOWN:
      RotateCamera(0,-1,0);
      break;
   }
}

/*
   Rotate (ix,iy) or roll (iz) the camera about the focal point
   ix,iy,iz are flags, 0 do nothing, +- 1 rotates in opposite directions
   Correctly updating all camera attributes
*/
void RotateCamera(int ix,int iy,int iz)
{
   XYZ vp,vu,vd;
   XYZ right;
   XYZ newvp,newr;
   double radius,dd,radians;
   double dx,dy,dz;

   vu = camera.vu;
   Normalise(&vu);
   vp = camera.vp;
   vd = camera.vd;
   Normalise(&vd);
   CROSSPROD(vd,vu,right);
   Normalise(&right);
   radians = dtheta * PI / 180.0;

   /* Handle the roll */
   if (iz != 0) {
      camera.vu.x += iz * right.x * radians;
      camera.vu.y += iz * right.y * radians;
      camera.vu.z += iz * right.z * radians;
      Normalise(&camera.vu);
      return;
   }

   /* Distance from the rotate point */
   dx = camera.vp.x - camera.pr.x;
   dy = camera.vp.y - camera.pr.y;
   dz = camera.vp.z - camera.pr.z;
   radius = sqrt(dx*dx + dy*dy + dz*dz);

   /* Determine the new view point */
   dd = radius * radians;
   newvp.x = vp.x + dd * ix * right.x + dd * iy * vu.x - camera.pr.x;
   newvp.y = vp.y + dd * ix * right.y + dd * iy * vu.y - camera.pr.y;
   newvp.z = vp.z + dd * ix * right.z + dd * iy * vu.z - camera.pr.z;
   Normalise(&newvp);
   camera.vp.x = camera.pr.x + radius * newvp.x;
   camera.vp.y = camera.pr.y + radius * newvp.y;
   camera.vp.z = camera.pr.z + radius * newvp.z;

   /* Determine the new right vector */
   newr.x = camera.vp.x + right.x - camera.pr.x;
   newr.y = camera.vp.y + right.y - camera.pr.y;
   newr.z = camera.vp.z + right.z - camera.pr.z;
   Normalise(&newr);
   newr.x = camera.pr.x + radius * newr.x - camera.vp.x;
   newr.y = camera.pr.y + radius * newr.y - camera.vp.y;
   newr.z = camera.pr.z + radius * newr.z - camera.vp.z;

   camera.vd.x = camera.pr.x - camera.vp.x;
   camera.vd.y = camera.pr.y - camera.vp.y;
   camera.vd.z = camera.pr.z - camera.vp.z;
   Normalise(&camera.vd);

   /* Determine the new up vector */
   CROSSPROD(newr,camera.vd,camera.vu);
   Normalise(&camera.vu);

   if (debug)
      fprintf(stderr,"Camera position: (%g,%g,%g)\n",
         camera.vp.x,camera.vp.y,camera.vp.z);
}

/*
   Fly the camera forwards or backwards
*/
void FlyCamera(int dir)
{
   double delta = 0.1;

   camera.vp.x = camera.vp.x + dir * camera.vd.x * delta;
   camera.vp.y = camera.vp.y + dir * camera.vd.y * delta;
   camera.vp.z = camera.vp.z + dir * camera.vd.z * delta;
}

/*
   Translate (pan) the camera view point
   In response to i,j,k,l keys
   Also move the camera rotate location in parallel
*/
void TranslateCamera(int ix,int iy)
{
   XYZ vp,vu,vd;
   XYZ right;
   XYZ newvp,newr;
   double radians,delta;

   vu = camera.vu;
   Normalise(&vu);
   vp = camera.vp;
   vd = camera.vd;
   Normalise(&vd);
   CROSSPROD(vd,vu,right);
   Normalise(&right);
   radians = dtheta * PI / 180.0;
   delta = dtheta * camera.focallength / 90.0;

   camera.vp.x += iy * vu.x * delta;
   camera.vp.y += iy * vu.y * delta;
   camera.vp.z += iy * vu.z * delta;
   camera.pr.x += iy * vu.x * delta;
   camera.pr.y += iy * vu.y * delta;
   camera.pr.z += iy * vu.z * delta;

   camera.vp.x += ix * right.x * delta;
   camera.vp.y += ix * right.y * delta;
   camera.vp.z += ix * right.z * delta;
   camera.pr.x += ix * right.x * delta;
   camera.pr.y += ix * right.y * delta;
   camera.pr.z += ix * right.z * delta;
}

/*
   Handle mouse events
   Right button events are passed to menu handlers
*/
void HandleMouse(int button,int state,int x,int y)
{
   if (state == GLUT_DOWN) {
      if (button == GLUT_LEFT_BUTTON) {
         currentbutton = GLUT_LEFT_BUTTON;
      } else if (button == GLUT_MIDDLE_BUTTON) {
         currentbutton = GLUT_MIDDLE_BUTTON;
      } 
   }
}

/*
   Handle the main menu
*/
void HandleMainMenu(int whichone)
{
   switch (whichone) {
   case 9: 
      exit(0); 
      break;
   }
}

/*
   Handle the font menu
*/
void HandleFontMenu(int whichone)
{
   whichfont = whichone - 1;
}

/*
   Handle the type menu
*/
void HandleStyleMenu(int whichone)
{
   whatstyle = whichone;
}

/*
   How to handle visibility
*/
void HandleVisibility(int visible)
{
   if (visible == GLUT_VISIBLE)
      glutIdleFunc(HandleIdle);
   else
      glutIdleFunc(NULL);
}

/*
   What to do on an idle event
*/
void HandleIdle(void)
{
   glutPostRedisplay();
}

/*
   Handle a window reshape/resize
*/
void HandleReshape(int w,int h)
{
   glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
   glViewport(0,0,(GLsizei)w,(GLsizei)h);
   screenwidth = w;
   screenheight = h;
}

/*
   Display the program usage information
*/
void GiveUsage(char *cmd)
{
   fprintf(stderr,"Usage: %s [-h] [-f] [-s] [-c]\n",cmd);
   fprintf(stderr,"          -h   this text\n");
   fprintf(stderr,"          -f   full screen\n");
   fprintf(stderr,"          -s   stereo\n");
   fprintf(stderr,"          -c   show construction lines\n");
   fprintf(stderr,"Key Strokes\n");
   fprintf(stderr,"  arrow keys   rotate left/right/up/down\n");
   fprintf(stderr,"  left mouse   rotate\n");
   fprintf(stderr,"middle mouse   roll\n");
   fprintf(stderr,"           c   toggle construction lines\n");
   fprintf(stderr,"           i   translate up\n");
   fprintf(stderr,"           k   translate down\n");
   fprintf(stderr,"           j   translate left\n");
   fprintf(stderr,"           l   translate right\n");
   fprintf(stderr,"           [   roll clockwise\n");
   fprintf(stderr,"           ]   roll anti clockwise\n");
   fprintf(stderr,"           +   move forwards\n");
   fprintf(stderr,"           -   move backwards\n");
   fprintf(stderr,"           q   quit\n");
   exit(-1);
}

void Normalise(XYZ *p)
{
   double length;

   length = sqrt(p->x * p->x + p->y * p->y + p->z * p->z);
   if (length != 0) {
      p->x /= length;
      p->y /= length;
      p->z /= length;
   } else {
      p->x = 0;
      p->y = 0;
      p->z = 0;
   }
}

XYZ CalcNormal(XYZ p,XYZ p1,XYZ p2)
{
   XYZ n,pa,pb;

   pa.x = p1.x - p.x;
   pa.y = p1.y - p.y;
   pa.z = p1.z - p.z;
   pb.x = p2.x - p.x;
   pb.y = p2.y - p.y;
   pb.z = p2.z - p.z;
   Normalise(&pa);
   Normalise(&pb);
  
   n.x = pa.y * pb.z - pa.z * pb.y;
   n.y = pa.z * pb.x - pa.x * pb.z;
   n.z = pa.x * pb.y - pa.y * pb.x;
   Normalise(&n);

   return(n);
}

/*
   Move the camera to the home position 
*/
void CameraHome(int mode)
{
   camera.aperture = 50;
   camera.focallength = 30;
   camera.eyesep = camera.focallength / 20;
   camera.pr = origin;

   camera.vp.x = camera.focallength;
   camera.vp.y = 0;
   camera.vp.z = 0;
   camera.vd.x = -camera.vp.x; 
   camera.vd.y = -camera.vp.y; 
   camera.vd.z = -camera.vp.z;

   camera.vu.x = 0;  
   camera.vu.y = 1; 
   camera.vu.z = 0;
}

/*
   Handle mouse motion
*/
void HandleMouseMotion(int x,int y)
{
   static int xlast=-1,ylast=-1;
   int dx,dy;

   dx = x - xlast;
   dy = y - ylast;
   if (dx < 0)      dx = -1;
   else if (dx > 0) dx =  1;
   if (dy < 0)      dy = -1;
   else if (dy > 0) dy =  1;

   if (currentbutton == GLUT_LEFT_BUTTON)
      RotateCamera(-dx,dy,0);
   else if (currentbutton == GLUT_MIDDLE_BUTTON)
      RotateCamera(0,0,dx);

   xlast = x;
   ylast = y;
}

/*
   Write the current view to a PPM file
   Do the right thing for stereo, ie: two images
*/
int WindowDump(int width,int height,int stereo)
{
   int i,j;
   FILE *fptr;
   static int counter = 0;
   char fname[32];
   unsigned char *image;

   /* Allocate our buffer for the image */
   if ((image = malloc(3*width*height*sizeof(char))) == NULL) {
      fprintf(stderr,"WindowDump - Failed to allocate memory for image\n");
      return(FALSE);
   }

   /* Open the file */
   sprintf(fname,"L_%04d.ppm",counter);
   if ((fptr = fopen(fname,"w")) == NULL) {
      fprintf(stderr,"WindowDump - Failed to open file for window dump\n");
      return(FALSE);
   }

   /* Copy the image into our buffer */
   glReadBuffer(GL_BACK_LEFT);
   glReadPixels(0,0,width,height,GL_RGB,GL_UNSIGNED_BYTE,image);

   /* Write the PPM file */
   fprintf(fptr,"P3\n%d %d\n255\n",width,height);
   for (j=height-1;j>=0;j--) {
      for (i=0;i<width;i++) {
         fputc(image[3*j*width+3*i+0],fptr);
         fputc(image[3*j*width+3*i+1],fptr);
         fputc(image[3*j*width+3*i+2],fptr);
      }
   }
   fclose(fptr);

   if (stereo) {

      /* Open the file */
      sprintf(fname,"R_%04d.ppm",counter);
      if ((fptr = fopen(fname,"w")) == NULL) {
         fprintf(stderr,"WindowDump - Failed to open file for window dump\n");
         return(FALSE);
      }

      /* Copy the image into our buffer */
      glReadBuffer(GL_BACK_RIGHT);
      glReadPixels(0,0,width,height,GL_RGB,GL_UNSIGNED_BYTE,image);

      /* Write the PPM file */
      fprintf(fptr,"P3\n%d %d\n255\n",width,height);
      for (j=height-1;j>=0;j--) {
         for (i=0;i<width;i++) {
            fputc(image[3*j*width+3*i+0],fptr);
            fputc(image[3*j*width+3*i+1],fptr);
            fputc(image[3*j*width+3*i+2],fptr);
         }
      }
      fclose(fptr);
   }

   free(image);
   counter++;
   return(TRUE);
}

