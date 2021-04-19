#version 450
out float vf0;
out float vf1;
layout(std140) uniform uBlock1 {
float u0;
float u4;
float u8;
float u12;
float u16;
float u20;
float u24;
float u28;
} u1;
in vec2 in0;
void main() {
vec2 t0 = in0;
float t1 = (t0.x+1);
float t2 = (t1/2);
vf0 = t2;
float t3 = (t0.y+1);
float t4 = (t3/2);
vf1 = t4;
float t5 = (t0.x*u1.u28);
float t6 = (1-u1.u28);
float t7 = (t5-t6);
float t8 = (t7+u1.u0);
float t9 = (t0.y*u1.u28);
float t10 = (t9-t6);
float t11 = (t10+u1.u4);
gl_Position = vec4(t8,t11,0,1);
}
