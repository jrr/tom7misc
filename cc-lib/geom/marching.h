// Marching cubes algorithm, which triangulates a 3D object given
// a signed distance field (as a C++ function) describing it.
// This is basically just a C++ wrapper around rjm_mc.h.

// THIS FILE ONLY:
// Use and distribute freely; see marching.cc for license.

#ifndef _CC_LIB_MARCHING_H
#define _CC_LIB_MARCHING_H

#include <functional>
#include <vector>
#include <tuple>

struct MarchingCubes {

  struct Pos {
    Pos() {}
    Pos(float x, float y, float z) : x(x), y(y), z(z) {}
    Pos(const Pos &other) = default;
    Pos(Pos &&other) = default;
    Pos &operator =(const Pos &other) = default;
    Pos &operator =(Pos &&other) = default;

    float x = 0.0f, y = 0.0f, z = 0.0f;
  };
  
  struct Vertex {
    Vertex() {}
    Vertex(Pos pos, Pos normal) : pos(pos), normal(normal) {}
    Vertex(const Vertex &other) = default;
    Vertex(Vertex &&other) = default;
    Vertex &operator =(const Vertex &other) = default;
    Vertex &operator =(Vertex &&other) = default;

    Pos pos;
    Pos normal;
  };
  
  struct Mesh {
    std::vector<Vertex> vertices;
    // Each triangle's three vertices are represented
    // as indices into the vertices vector.
    std::vector<std::tuple<int, int, int>> triangles;
  };

  // bound_min and _max are the corners of the bounding box to
  //   triangulate within.
  // cellsize is the dimension of the marching cubes. Smaller
  //   values yield a denser mesh.
  // shape is the signed distance function defining the object.
  //   It should return the distance to the surface, with
  //   positive meaning outside and negative meaning inside.
  static Mesh Generate(Pos bound_min, Pos bound_max,
                       float cellsize,
                       const std::function<float(Pos)> &shape);
};
  
#endif
