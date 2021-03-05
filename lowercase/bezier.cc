// THIS FILE ONLY:
//
// This is based on public domain code from "Graphics Gems."
// (Andrew S. Glassner, published by Academic Press,
// Cambridge, MA, 1990, ISBN 0-12-286165-5)
//
// "The authors and the publisher hold no copyright restrictions on any
// of these files; this source code is public domain, and is freely
// available to the entire computer graphics community for study, use,
// and modification. We do request that the comment at the top of each
// file, identifying the original author and its original publication
// in the book Graphics Gems, be retained in all programs that use
// these files."
//
// These algorithms are from
// "Solving the Nearest Point-on-Curve Problem"
// and
// "A Bezier Curve-Based Root-Finder"
// by Philip J. Schneider
//
// Ported to C++ by Tom Murphy VII in 2021. Also released to
// the public domain.

#include <cmath>
#include <tuple>
// #include "GraphicsGems.h"

using namespace std;

#define TESTMODE

struct Point2 {
  float x = 0.0f;
  float y = 0.0f;
};

using Vector2 = Point2;

/* return vector difference c = a-b */
inline static Vector2 *V2Sub(Vector2 *a, Vector2 *b, Vector2 *c) {
  c->x = a->x-b->x;  c->y = a->y-b->y;
  return c;
}

inline static float V2SquaredLength(Vector2 *a)  {
  return (a->x * a->x) + (a->y * a->y);
}

inline static float V2Dot(Vector2 *a, Vector2 *b) {
  return (a->x*b->x)+(a->y*b->y);
}

// XXX use STL
#define MIN(a,b)	(((a)<(b))?(a):(b))
#define MAX(a,b)	(((a)>(b))?(a):(b))

/*
uh these would need params
Point2  NearestPointOnCurve();
static  int     FindRoots();
static  Point2  *ConvertToBezierForm();
static  double  ComputeXIntercept();
static  int     ControlPolygonFlatEnough();
static  int     CrossingCount();
static  Point2  Bezier();
static  Vector2 V2ScaleII();
*/

// XXX parameterize on floats!

// DEGREE 2 = quadratic bezier (one control point)
// DEGREE 3 = cubic bezier
template<int DEGREE>
struct BezierImpl {
  /*  Maximum depth for recursion */
  static constexpr int MAXDEPTH = 64;
  /* Flatness control value */
  static constexpr float EPSILON = ldexp(1.0,-MAXDEPTH-1);

  /*  Degree of eqn to find roots of. */
  static constexpr int W_DEGREE = 2 * DEGREE - 1;

  static inline Vector2 V2ScaleII(Vector2 *v, float s) {
    Vector2 result;

    result.x = v->x * s; result.y = v->y * s;
    return result;
  }

  /*
   *  NearestPointOnCurve :
   *      Compute the parameter value of the point on a Bezier
   *              curve segment closest to some arbtitrary, user-input point.
   *              Return the point on the curve at that parameter value.
   *
   */
  static Point2 NearestPointOnCurve(
      /* The user-supplied point    */
      Point2 P,
      /* Control points of Bezier */
      Point2 *V,
      // output dist to closest point
      float *dist,
      // output t where closest point is
      float *t) {
    Point2      *w;                     /* Ctl pts for 5th-degree eqn   */
    float      t_candidate[W_DEGREE];  /* Possible roots               */
    int         n_solutions;            /* Number of roots found        */


    /*  Convert problem to 5th-degree Bezier form       */
    w = ConvertToBezierForm(P, V);

    /* Find all possible roots of 5th-degree equation */
    n_solutions = FindRoots(w, W_DEGREE, t_candidate, 0);
    free((char *)w);

    /* Compare distances of P to all candidates, and to t=0, and t=1 */
    {
      float  new_dist;
      Point2  p;
      Vector2  v;
      int             i;


      /* Check distance to beginning of curve, where t = 0    */
      *dist = V2SquaredLength(V2Sub(&P, &V[0], &v));
      *t = 0.0;

      /* Find distances for candidate points  */
      for (i = 0; i < n_solutions; i++) {
        p = Bezier(V, DEGREE, t_candidate[i],
                   (Point2 *)NULL, (Point2 *)NULL);
        new_dist = V2SquaredLength(V2Sub(&P, &p, &v));
        if (new_dist < *dist) {
          *dist = new_dist;
          *t = t_candidate[i];
        }
      }

      /* Finally, look at distance to end point, where t = 1.0 */
      new_dist = V2SquaredLength(V2Sub(&P, &V[DEGREE], &v));
      if (new_dist < *dist) {
        *dist = new_dist;
        *t = 1.0;
      }
    }

    /*  Return the point on the curve at parameter value t */
    // printf("t : %4.12f\n", t);
    return Bezier(V, DEGREE, *t, (Point2 *)NULL, (Point2 *)NULL);
  }


  /*
   *  ConvertToBezierForm :
   *              Given a point and a Bezier curve, generate a 5th-degree
   *              Bezier-format equation whose solution finds the point on the
   *      curve nearest the user-defined point.
   */
  static Point2 *ConvertToBezierForm(
      /* The point to find t for      */
      Point2 P,
      /* The control points           */
      Point2      *V) {
    int         i, j, k, m, n, ub, lb;
    int         row, column;            /* Table indices                */
    Vector2     c[DEGREE+1];            /* V(i)'s - P                   */
    Vector2     d[DEGREE];              /* V(i+1) - V(i)                */
    Point2      *w;                     /* Ctl pts of 5th-degree curve  */
    /* Dot product of c, d          */
    // float      cdTable[3][4];
    float cdTable[DEGREE][DEGREE + 1];

    static constexpr float QUAD_Z[2][3] = {
      {1.0f, 2.0f/3.0f, 1.0f/3.0f},
      {1.0f/3.0f, 2.0f/3.0f, 1.0f},
    };

    static constexpr float CUBIC_Z[3][4] = {
      {1.0, 0.6, 0.3, 0.1},
      {0.4, 0.6, 0.6, 0.4},
      {0.1, 0.3, 0.6, 1.0},
    };

    auto Z = [](int r, int c) {
        static_assert(DEGREE == 2 || DEGREE == 3, "unsupported degree");
        if constexpr (DEGREE == 2) {
          return QUAD_Z[r][c];
        } else if constexpr (DEGREE == 3) {
          return CUBIC_Z[r][c];
        }
      };

    /*Determine the c's -- these are vectors created by subtracting*/
    /* point P from each of the control points                          */
    for (i = 0; i <= DEGREE; i++) {
      V2Sub(&V[i], &P, &c[i]);
    }
    /* Determine the d's -- these are vectors created by subtracting*/
    /* each control point from the next                                 */
    for (i = 0; i <= DEGREE - 1; i++) {
      d[i] = V2ScaleII(V2Sub(&V[i+1], &V[i], &d[i]), 3.0);
    }

    /* Create the c,d table -- this is a table of dot products of the */
    /* c's and d's                                                      */
    for (row = 0; row <= DEGREE - 1; row++) {
      for (column = 0; column <= DEGREE; column++) {
        cdTable[row][column] = V2Dot(&d[row], &c[column]);
      }
    }

    /* Now, apply the z's to the dot products, on the skew diagonal*/
    /* Also, set up the x-values, making these "points"         */
    w = (Point2 *)malloc((unsigned)(W_DEGREE+1) * sizeof(Point2));
    for (i = 0; i <= W_DEGREE; i++) {
      w[i].y = 0.0;
      w[i].x = (float)(i) / W_DEGREE;
    }

    n = DEGREE;
    m = DEGREE-1;
    for (k = 0; k <= n + m; k++) {
      lb = MAX(0, k - m);
      ub = MIN(k, n);
      for (i = lb; i <= ub; i++) {
        j = k - i;
        w[i+j].y += cdTable[j][i] * Z(j, i);
      }
    }

    return (w);
  }


  /*
   *  FindRoots :
   *      Given a 5th-degree equation in Bernstein-Bezier form, find
   *      all of the roots in the interval [0, 1].  Return the number
   *      of roots found.
   */
  static int FindRoots(
      /* The control points           */
      Point2      *w,
      /* The degree of the polynomial */
      int         degree,
      /* RETURN candidate t-values    */
      float      *t,
      /* The depth of the recursion   */
      int         depth) {
    int         i;
    /* New left and right control polygons. */
    Point2      Left[W_DEGREE+1], Right[W_DEGREE+1];
    /* Solution count from children */
    int         left_count, right_count;
    /* Solutions from kids          */
    float      left_t[W_DEGREE+1], right_t[W_DEGREE+1];

    switch (CrossingCount(w, degree)) {
    case 0 : {      /* No solutions here    */
      return 0;
    }
    case 1 : {      /* Unique solution      */
      /* Stop recursion when the tree is deep enough      */
      /* if deep enough, return 1 solution at midpoint    */
      if (depth >= MAXDEPTH) {
        t[0] = (w[0].x + w[W_DEGREE].x) / 2.0;
        return 1;
      }
      if (ControlPolygonFlatEnough(w, degree)) {
        t[0] = ComputeXIntercept(w, degree);
        return 1;
      }
      break;
    }
    }

    /* Otherwise, solve recursively after
       subdividing control polygon              */
    Bezier(w, degree, 0.5, Left, Right);
    left_count  = FindRoots(Left,  degree, left_t, depth+1);
    right_count = FindRoots(Right, degree, right_t, depth+1);


    /* Gather solutions together        */
    for (i = 0; i < left_count; i++) {
      t[i] = left_t[i];
    }
    for (i = 0; i < right_count; i++) {
      t[i+left_count] = right_t[i];
    }

    /* Send back total number of solutions      */
    return (left_count+right_count);
  }


  /* take binary sign of a, either -1, or 1 if >= 0 */
  #define SGN(a)		(((a)<0) ? -1 : 1)

  /*
   * CrossingCount :
   *      Count the number of times a Bezier control polygon
   *      crosses the 0-axis. This number is >= the number of roots.
   *
   */
  static int CrossingCount(
      /*  Control pts of Bezier curve */
      Point2      *V,
      /*  Degreee of Bezier curve     */
      int         degree) {
    int         i;
    int         n_crossings = 0;        /*  Number of zero-crossings    */
    int         sign, old_sign;         /*  Sign of coefficients        */

    sign = old_sign = SGN(V[0].y);
    for (i = 1; i <= degree; i++) {
      sign = SGN(V[i].y);
      if (sign != old_sign) n_crossings++;
      old_sign = sign;
    }
    return n_crossings;
  }



  /*
   *  ControlPolygonFlatEnough :
   *      Check if the control polygon of a Bezier curve is flat enough
   *      for recursive subdivision to bottom out.
   *
   *  Corrections by James Walker, jw@jwwalker.com, as follows:
  There seem to be errors in the ControlPolygonFlatEnough function in the
  Graphics Gems book and the repository (NearestPoint.c). This function
  is briefly described on p. 413 of the text, and appears on pages 793-794.
  I see two main problems with it.
  The idea is to find an upper bound for the error of approximating the x
  intercept of the Bezier curve by the x intercept of the line through the
  first and last control points. It is claimed on p. 413 that this error is
  bounded by half of the difference between the intercepts of the bounding
  box. I don't see why that should be true. The line joining the first and
  last control points can be on one side of the bounding box, and the actual
  curve can be near the opposite side, so the bound should be the difference
  of the bounding box intercepts, not half of it.
  Second, we come to the implementation. The values distance[i] computed in
  the first loop are not actual distances, but squares of distances. I
  realize that minimizing or maximizing the squares is equivalent to
  minimizing or maximizing the distances.  But when the code claims that
  one of the sides of the bounding box has equation
  a * x + b * y + c + max_distance_above, where max_distance_above is one of
  those squared distances, that makes no sense to me.
  I have appended my version of the function. If you apply my code to the
  cubic Bezier curve used to test NearestPoint.c,
   static Point2 bezCurve[4] = {    /  A cubic Bezier curve    /
      { 0.0, 0.0 },
      { 1.0, 2.0 },
      { 3.0, 3.0 },
      { 4.0, 2.0 },
      };
  my code computes left_intercept = -3.0 and right_intercept = 0.0, which you
  can verify by sketching a graph. The original code computes
  left_intercept = 0.0 and right_intercept = 0.9.
   */

  /* static int ControlPolygonFlatEnough( const Point2* V, int degree ) */
  static int ControlPolygonFlatEnough(
      /* Control points       */
      Point2 *V,
      /* Degree of polynomial */
      int degree) {
    /* Index variable        */
    int     i;
    float  value;
    float  max_distance_above;
    float  max_distance_below;
    float  error;        /* Precision of root        */
    float  intercept_1,
      intercept_2,
      left_intercept,
      right_intercept;
    /* Coefficients of implicit
       eqn for line from V[0]-V[deg]*/
    float  a, b, c;

    float  det, dInv;
    float  a1, b1, c1, a2, b2, c2;

    /* Derive the implicit equation for line connecting first and last
       control points */
    a = V[0].y - V[degree].y;
    b = V[degree].x - V[0].x;
    c = V[0].x * V[degree].y - V[degree].x * V[0].y;

    max_distance_above = max_distance_below = 0.0;

    for (i = 1; i < degree; i++) {
      value = a * V[i].x + b * V[i].y + c;

      if (value > max_distance_above) {
        max_distance_above = value;
      } else if (value < max_distance_below) {
        max_distance_below = value;
      }
    }

    /*  Implicit equation for zero line */
    a1 = 0.0;
    b1 = 1.0;
    c1 = 0.0;

    /*  Implicit equation for "above" line */
    a2 = a;
    b2 = b;
    c2 = c - max_distance_above;

    det = a1 * b2 - a2 * b1;
    dInv = 1.0/det;

    intercept_1 = (b1 * c2 - b2 * c1) * dInv;

    /*  Implicit equation for "below" line */
    a2 = a;
    b2 = b;
    c2 = c - max_distance_below;

    det = a1 * b2 - a2 * b1;
    dInv = 1.0/det;

    intercept_2 = (b1 * c2 - b2 * c1) * dInv;

    /* Compute intercepts of bounding box    */
    left_intercept = MIN(intercept_1, intercept_2);
    right_intercept = MAX(intercept_1, intercept_2);

    error = right_intercept - left_intercept;

    return (error < EPSILON) ? 1 : 0;
  }


  /*
   *  ComputeXIntercept :
   *      Compute intersection of chord from first control point to last
   *      with 0-axis.
   *
   */
  /* NOTE: "T" and "Y" do not have to be computed, and there are many useless
   * operations in the following (e.g. "0.0 - 0.0").
   */
  static float ComputeXIntercept(
      /*  Control points      */
      Point2      *V,
      /*  Degree of curve     */
      int         degree) {
    float      XLK, YLK, XNM, YNM, XMK, YMK;
    float      det, detInv;
    float      S;
    float      X;

    XLK = 1.0 - 0.0;
    YLK = 0.0 - 0.0;
    XNM = V[degree].x - V[0].x;
    YNM = V[degree].y - V[0].y;
    XMK = V[0].x - 0.0;
    YMK = V[0].y - 0.0;

    det = XNM*YLK - YNM*XLK;
    detInv = 1.0/det;

    S = (XNM*YMK - YNM*XMK) * detInv;
    /*  T = (XLK*YMK - YLK*XMK) * detInv; */

    X = 0.0 + XLK * S;
    /*  Y = 0.0 + YLK * S; */

    return X;
  }


  /*
   *  Bezier :
   *      Evaluate a Bezier curve at a particular parameter value
   *      Fill in control points for resulting sub-curves if "Left" and
   *      "Right" are non-null.
   *
   */
  static Point2 Bezier(
      /* Control pts                  */
      Point2      *V,
      /* Degree of bezier curve       */
      int         degree,
      /* Parameter value              */
      float      t,
      /* RETURN left half ctl pts     */
      Point2      *Left,
      /* RETURN right half ctl pts    */
      Point2      *Right) {
    /* Index variables      */
    int         i, j;
    Point2      Vtemp[W_DEGREE+1][W_DEGREE+1];


    /* Copy control points      */
    for (j =0; j <= degree; j++) {
      Vtemp[0][j] = V[j];
    }

    /* Triangle computation     */
    for (i = 1; i <= degree; i++) {
      for (j =0 ; j <= degree - i; j++) {
        Vtemp[i][j].x =
          (1.0 - t) * Vtemp[i-1][j].x + t * Vtemp[i-1][j+1].x;
        Vtemp[i][j].y =
          (1.0 - t) * Vtemp[i-1][j].y + t * Vtemp[i-1][j+1].y;
      }
    }

    if (Left != NULL) {
      for (j = 0; j <= degree; j++) {
        Left[j]  = Vtemp[j][0];
      }
    }
    if (Right != NULL) {
      for (j = 0; j <= degree; j++) {
        Right[j] = Vtemp[degree-j][j];
      }
    }

    return (Vtemp[degree][0]);
  }
};

 #if 0
/*
 *  main :
 *      Given a cubic Bezier curve (i.e., its control points), and some
 *      arbitrary point in the plane, find the point on the curve
 *      closest to that arbitrary point.
 */
int main()
{

 static Point2 bezCurve[4] = {  /*  A cubic Bezier curve        */
        { 0.0, 0.0 },
        { 1.0, 2.0 },
        { 3.0, 3.0 },
        { 4.0, 2.0 },
    };
    static Point2 arbPoint = { 3.5, 2.0 }; /*Some arbitrary point*/
    Point2      pointOnCurve;            /*  Nearest point on the curve */

    /*  Find the closest point */
    pointOnCurve = NearestPointOnCurve(arbPoint, bezCurve);
    printf("pointOnCurve : (%4.4f, %4.4f)\n", pointOnCurve.x,
                pointOnCurve.y);
}
#endif /* TESTMODE */

std::tuple<float, float, float>
DistanceFromPointToQuadBezier(
    // The point to test
    float px, float py,
    // Bezier start point
    float sx, float sy,
    // Bezier ontrol point
    float cx, float cy,
    // Bezier end point
    float ex, float ey) {
  Point2 p{.x = px, .y = py};
  Point2 v[3] = {{.x = sx, .y = sy},
                 {.x = cx, .y = cy},
                 {.x = ex, .y = ey}};

  [[maybe_unused]] float t;
  float dist;
  Point2 closest =
    BezierImpl<2>::NearestPointOnCurve(p, v, &dist, &t);
  return make_tuple(closest.x, closest.y, dist);
}
