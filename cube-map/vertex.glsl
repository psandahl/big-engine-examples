#version 330 core

layout (location = 0) in vec3 position;

uniform mat4 vp;
uniform mat4 model;

out vec3 vTexCoord;

void main()
{
  vTexCoord = (model * vec4(position, 1.0)).xyz; 
  gl_Position = vp * model * vec4(position, 1.0);
}
