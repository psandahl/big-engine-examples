#version 330 core

in vec3 vTexCoord;

uniform samplerCube cube;

out vec4 color;

bool isZ(vec3 v);

void main()
{
  if (isZ(vTexCoord))
  {
    // Flip x-axis if it is a front or back face fragment ...
    vec3 texCoord = vec3(-vTexCoord.x, vTexCoord.y, vTexCoord.z);
    color = vec4(textureCube(cube, texCoord).rgb, 1);
  }
  else
  {
    // Otherwise flip the x-axis.
    vec3 texCoord = vec3(vTexCoord.x, vTexCoord.y, -vTexCoord.z);
    color = vec4(textureCube(cube, texCoord).rgb, 1);
  }
}

bool isZ(vec3 v)
{
  return length(v.z) >= length(v.x) && length(v.z) >= length(v.y);
}
