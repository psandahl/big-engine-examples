#version 330 core

in vec2 vTexCoord;

uniform sampler2D catTexture;

out vec4 color;

void main()
{
  vec4 pixel = texture2D(catTexture, vec2(vTexCoord.s, 1 - vTexCoord.t));
  if (pixel.a < 0.5)
  {
    discard;
  }
  else
  {
    color = pixel;
  }
}
