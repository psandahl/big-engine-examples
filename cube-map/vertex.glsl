#version 330 core

layout (location = 0) in vec3 position;

uniform mat4 mvp;
uniform mat4 model;

out vec3 vTexCoord;

void main()
{
  //vTexCoord = (model * vec4(position, 1.0)).xyz;
  vTexCoord = position;
  gl_Position = mvp * vec4(position, 1.0);
}
