#version 330 core

in vec2 vTexCoord;

uniform sampler2D tex;

out vec4 color;

void main()
{
  color = vec4(texture2D(tex, vTexCoord).rgb, 1.0);
}
