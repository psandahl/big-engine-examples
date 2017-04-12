#version 330 core

in vec3 vTexCoord;

uniform samplerCube cube;

out vec4 color;

void main()
{
  color = vec4(textureCube(cube, vTexCoord).rgb, 1);
}
