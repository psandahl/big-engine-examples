#version 330 core

in vec2 vTexCoord;

uniform sampler2D catTexture;

out vec4 color;

void main()
{
  color = vec4(texture(catTexture, vTexCoord).rgb, 1.0);
}
