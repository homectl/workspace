#version 450
layout(location = 0) out vec4 out0;
layout(location = 1) out vec4 out1;
layout(std140) uniform uBlock5 {
float u0;
float u4;
float u8;
float u12;
float u16;
float u20;
float u24;
} u5;
layout(std140) uniform uBlock6 {
float u0;
float u4;
float u8;
float u12;
float u16;
float u20;
float u24;
float u28;
} u6;
layout(std140) uniform uBlock7 {
float u0;
float u4;
float u8;
float u12;
float u16;
float u20;
} u7;
layout(std140) uniform uBlock8 {
float u0;
float u4;
float u8;
float u12;
float u16;
float u20;
} u8;
uniform sampler2D s10;
uniform sampler2D s11;
uniform sampler2D s12;
in float vf0;
in float vf1;
in float vf2;
in float vf3;
in float vf4;
in float vf5;
in float vf6;
in float vf7;
in float vf8;
in float vf9;
in float vf10;
in float vf11;
in float vf12;
in float vf13;
in float vf14;
in float vf15;
in float vf16;
in float vf17;
void main() {
vec4 t0 = vec4(u7.u0,u7.u4,u7.u8,1);
float t1 = (u5.u0/2);
float t2 = cos(t1);
float t3 = sin(t1);
float t4 = (-t3);
vec4 t5 = vec4(t2,t4,0,0);
float t6 = sin(t1);
vec4 t7 = vec4(t6,t2,0,0);
vec4 t8 = vec4(0,0,1,0);
vec4 t9 = vec4(0,0,0,1);
mat4x4 t10 = mat4x4(t5,t7,t8,t9);
vec4 t11 = (t0*t10);
float t12 = (t11[0]-vf2);
float t13 = (t12*t12);
float t14 = (t11[1]-vf3);
float t15 = (t14*t14);
float t16 = (t13+t15);
float t17 = (t11[2]-vf4);
float t18 = (t17*t17);
float t19 = (t16+t18);
float t20 = (t11[3]-vf5);
float t21 = (t20*t20);
float t22 = (t19+t21);
float t23 = sqrt(t22);
float t24 = (t12/t23);
float t25 = (vf6*t24);
float t26 = (t14/t23);
float t27 = (vf7*t26);
float t28 = (t25+t27);
float t29 = (t17/t23);
float t30 = (vf8*t29);
float t31 = (t28+t30);
float t32 = (t20/t23);
float t33 = (vf9*t32);
float t34 = (t31+t33);
bool t35 = (t34>=0);
float t36;
if(t35){
t36 = t34;
} else {
t36 = 0;
}
float t37 = (u7.u12*t36);
float t38 = (vf12/vf13);
float t39 = (t38*float(0.5));
float t40 = (t39+float(0.5));
vec4 t41 = vec4(vf6,vf7,vf8,vf9);
float t42 = (t11[0]-vf2);
float t43 = (t11[1]-vf3);
float t44 = (t11[2]-vf4);
float t45 = (t11[3]-vf5);
vec4 t46 = vec4(t42,t43,t44,t45);
vec4 t47 = normalize(t46);
vec4 t48 = vec4(t47[0],t47[1],t47[2],t47[3]);
float t49 = dot(t41,t48);
float t50 = (float(1.0)-t49);
float t51 = (float(5.0e-4)*t50);
bool t52 = (t51>=float(5.0e-5));
float t53;
if(t52){
t53 = t51;
} else {
t53 = float(5.0e-5);
}
float t54 = (t40-t53);
float t55 = (vf10/vf13);
float t56 = (t55*float(0.5));
float t57 = (t56+float(0.5));
float t58 = (float(1.0)/600);
float t59 = (0*t58);
float t60 = (t57+t59);
float t61 = (vf11/vf13);
float t62 = (t61*float(0.5));
float t63 = (t62+float(0.5));
float t64 = (0*t58);
float t65 = (t63+t64);
vec4 t66 = texture(s10,vec2(t60,t65));
bool t67 = (t54>t66.x);
float t68;
if(t67){
t68 = float(0.1);
} else {
t68 = float(1.0);
}
float t69 = (0+t68);
float t70 = (vf12/vf13);
float t71 = (t70*float(0.5));
float t72 = (t71+float(0.5));
vec4 t73 = vec4(vf6,vf7,vf8,vf9);
float t74 = (t11[0]-vf2);
float t75 = (t11[1]-vf3);
float t76 = (t11[2]-vf4);
float t77 = (t11[3]-vf5);
vec4 t78 = vec4(t74,t75,t76,t77);
vec4 t79 = normalize(t78);
vec4 t80 = vec4(t79[0],t79[1],t79[2],t79[3]);
float t81 = dot(t73,t80);
float t82 = (float(1.0)-t81);
float t83 = (float(5.0e-4)*t82);
bool t84 = (t83>=float(5.0e-5));
float t85;
if(t84){
t85 = t83;
} else {
t85 = float(5.0e-5);
}
float t86 = (t72-t85);
float t87 = (vf10/vf13);
float t88 = (t87*float(0.5));
float t89 = (t88+float(0.5));
float t90 = (-1);
float t91 = (t90*t58);
float t92 = (t89+t91);
float t93 = (vf11/vf13);
float t94 = (t93*float(0.5));
float t95 = (t94+float(0.5));
float t96 = (t90*t58);
float t97 = (t95+t96);
vec4 t98 = texture(s10,vec2(t92,t97));
bool t99 = (t86>t98.x);
float t100;
if(t99){
t100 = float(0.1);
} else {
t100 = float(1.0);
}
float t101 = (t69+t100);
float t102 = (vf12/vf13);
float t103 = (t102*float(0.5));
float t104 = (t103+float(0.5));
vec4 t105 = vec4(vf6,vf7,vf8,vf9);
float t106 = (t11[0]-vf2);
float t107 = (t11[1]-vf3);
float t108 = (t11[2]-vf4);
float t109 = (t11[3]-vf5);
vec4 t110 = vec4(t106,t107,t108,t109);
vec4 t111 = normalize(t110);
vec4 t112 = vec4(t111[0],t111[1],t111[2],t111[3]);
float t113 = dot(t105,t112);
float t114 = (float(1.0)-t113);
float t115 = (float(5.0e-4)*t114);
bool t116 = (t115>=float(5.0e-5));
float t117;
if(t116){
t117 = t115;
} else {
t117 = float(5.0e-5);
}
float t118 = (t104-t117);
float t119 = (vf10/vf13);
float t120 = (t119*float(0.5));
float t121 = (t120+float(0.5));
float t122 = (-1);
float t123 = (t122*t58);
float t124 = (t121+t123);
float t125 = (vf11/vf13);
float t126 = (t125*float(0.5));
float t127 = (t126+float(0.5));
float t128 = (0*t58);
float t129 = (t127+t128);
vec4 t130 = texture(s10,vec2(t124,t129));
bool t131 = (t118>t130.x);
float t132;
if(t131){
t132 = float(0.1);
} else {
t132 = float(1.0);
}
float t133 = (t101+t132);
float t134 = (vf12/vf13);
float t135 = (t134*float(0.5));
float t136 = (t135+float(0.5));
vec4 t137 = vec4(vf6,vf7,vf8,vf9);
float t138 = (t11[0]-vf2);
float t139 = (t11[1]-vf3);
float t140 = (t11[2]-vf4);
float t141 = (t11[3]-vf5);
vec4 t142 = vec4(t138,t139,t140,t141);
vec4 t143 = normalize(t142);
vec4 t144 = vec4(t143[0],t143[1],t143[2],t143[3]);
float t145 = dot(t137,t144);
float t146 = (float(1.0)-t145);
float t147 = (float(5.0e-4)*t146);
bool t148 = (t147>=float(5.0e-5));
float t149;
if(t148){
t149 = t147;
} else {
t149 = float(5.0e-5);
}
float t150 = (t136-t149);
float t151 = (vf10/vf13);
float t152 = (t151*float(0.5));
float t153 = (t152+float(0.5));
float t154 = (-1);
float t155 = (t154*t58);
float t156 = (t153+t155);
float t157 = (vf11/vf13);
float t158 = (t157*float(0.5));
float t159 = (t158+float(0.5));
float t160 = (1*t58);
float t161 = (t159+t160);
vec4 t162 = texture(s10,vec2(t156,t161));
bool t163 = (t150>t162.x);
float t164;
if(t163){
t164 = float(0.1);
} else {
t164 = float(1.0);
}
float t165 = (t133+t164);
float t166 = (vf12/vf13);
float t167 = (t166*float(0.5));
float t168 = (t167+float(0.5));
vec4 t169 = vec4(vf6,vf7,vf8,vf9);
float t170 = (t11[0]-vf2);
float t171 = (t11[1]-vf3);
float t172 = (t11[2]-vf4);
float t173 = (t11[3]-vf5);
vec4 t174 = vec4(t170,t171,t172,t173);
vec4 t175 = normalize(t174);
vec4 t176 = vec4(t175[0],t175[1],t175[2],t175[3]);
float t177 = dot(t169,t176);
float t178 = (float(1.0)-t177);
float t179 = (float(5.0e-4)*t178);
bool t180 = (t179>=float(5.0e-5));
float t181;
if(t180){
t181 = t179;
} else {
t181 = float(5.0e-5);
}
float t182 = (t168-t181);
float t183 = (vf10/vf13);
float t184 = (t183*float(0.5));
float t185 = (t184+float(0.5));
float t186 = (0*t58);
float t187 = (t185+t186);
float t188 = (vf11/vf13);
float t189 = (t188*float(0.5));
float t190 = (t189+float(0.5));
float t191 = (-1);
float t192 = (t191*t58);
float t193 = (t190+t192);
vec4 t194 = texture(s10,vec2(t187,t193));
bool t195 = (t182>t194.x);
float t196;
if(t195){
t196 = float(0.1);
} else {
t196 = float(1.0);
}
float t197 = (t165+t196);
float t198 = (vf12/vf13);
float t199 = (t198*float(0.5));
float t200 = (t199+float(0.5));
vec4 t201 = vec4(vf6,vf7,vf8,vf9);
float t202 = (t11[0]-vf2);
float t203 = (t11[1]-vf3);
float t204 = (t11[2]-vf4);
float t205 = (t11[3]-vf5);
vec4 t206 = vec4(t202,t203,t204,t205);
vec4 t207 = normalize(t206);
vec4 t208 = vec4(t207[0],t207[1],t207[2],t207[3]);
float t209 = dot(t201,t208);
float t210 = (float(1.0)-t209);
float t211 = (float(5.0e-4)*t210);
bool t212 = (t211>=float(5.0e-5));
float t213;
if(t212){
t213 = t211;
} else {
t213 = float(5.0e-5);
}
float t214 = (t200-t213);
float t215 = (vf10/vf13);
float t216 = (t215*float(0.5));
float t217 = (t216+float(0.5));
float t218 = (0*t58);
float t219 = (t217+t218);
float t220 = (vf11/vf13);
float t221 = (t220*float(0.5));
float t222 = (t221+float(0.5));
float t223 = (1*t58);
float t224 = (t222+t223);
vec4 t225 = texture(s10,vec2(t219,t224));
bool t226 = (t214>t225.x);
float t227;
if(t226){
t227 = float(0.1);
} else {
t227 = float(1.0);
}
float t228 = (t197+t227);
float t229 = (vf12/vf13);
float t230 = (t229*float(0.5));
float t231 = (t230+float(0.5));
vec4 t232 = vec4(vf6,vf7,vf8,vf9);
float t233 = (t11[0]-vf2);
float t234 = (t11[1]-vf3);
float t235 = (t11[2]-vf4);
float t236 = (t11[3]-vf5);
vec4 t237 = vec4(t233,t234,t235,t236);
vec4 t238 = normalize(t237);
vec4 t239 = vec4(t238[0],t238[1],t238[2],t238[3]);
float t240 = dot(t232,t239);
float t241 = (float(1.0)-t240);
float t242 = (float(5.0e-4)*t241);
bool t243 = (t242>=float(5.0e-5));
float t244;
if(t243){
t244 = t242;
} else {
t244 = float(5.0e-5);
}
float t245 = (t231-t244);
float t246 = (vf10/vf13);
float t247 = (t246*float(0.5));
float t248 = (t247+float(0.5));
float t249 = (1*t58);
float t250 = (t248+t249);
float t251 = (vf11/vf13);
float t252 = (t251*float(0.5));
float t253 = (t252+float(0.5));
float t254 = (-1);
float t255 = (t254*t58);
float t256 = (t253+t255);
vec4 t257 = texture(s10,vec2(t250,t256));
bool t258 = (t245>t257.x);
float t259;
if(t258){
t259 = float(0.1);
} else {
t259 = float(1.0);
}
float t260 = (t228+t259);
float t261 = (vf12/vf13);
float t262 = (t261*float(0.5));
float t263 = (t262+float(0.5));
vec4 t264 = vec4(vf6,vf7,vf8,vf9);
float t265 = (t11[0]-vf2);
float t266 = (t11[1]-vf3);
float t267 = (t11[2]-vf4);
float t268 = (t11[3]-vf5);
vec4 t269 = vec4(t265,t266,t267,t268);
vec4 t270 = normalize(t269);
vec4 t271 = vec4(t270[0],t270[1],t270[2],t270[3]);
float t272 = dot(t264,t271);
float t273 = (float(1.0)-t272);
float t274 = (float(5.0e-4)*t273);
bool t275 = (t274>=float(5.0e-5));
float t276;
if(t275){
t276 = t274;
} else {
t276 = float(5.0e-5);
}
float t277 = (t263-t276);
float t278 = (vf10/vf13);
float t279 = (t278*float(0.5));
float t280 = (t279+float(0.5));
float t281 = (1*t58);
float t282 = (t280+t281);
float t283 = (vf11/vf13);
float t284 = (t283*float(0.5));
float t285 = (t284+float(0.5));
float t286 = (0*t58);
float t287 = (t285+t286);
vec4 t288 = texture(s10,vec2(t282,t287));
bool t289 = (t277>t288.x);
float t290;
if(t289){
t290 = float(0.1);
} else {
t290 = float(1.0);
}
float t291 = (t260+t290);
float t292 = (vf12/vf13);
float t293 = (t292*float(0.5));
float t294 = (t293+float(0.5));
vec4 t295 = vec4(vf6,vf7,vf8,vf9);
float t296 = (t11[0]-vf2);
float t297 = (t11[1]-vf3);
float t298 = (t11[2]-vf4);
float t299 = (t11[3]-vf5);
vec4 t300 = vec4(t296,t297,t298,t299);
vec4 t301 = normalize(t300);
vec4 t302 = vec4(t301[0],t301[1],t301[2],t301[3]);
float t303 = dot(t295,t302);
float t304 = (float(1.0)-t303);
float t305 = (float(5.0e-4)*t304);
bool t306 = (t305>=float(5.0e-5));
float t307;
if(t306){
t307 = t305;
} else {
t307 = float(5.0e-5);
}
float t308 = (t294-t307);
float t309 = (vf10/vf13);
float t310 = (t309*float(0.5));
float t311 = (t310+float(0.5));
float t312 = (1*t58);
float t313 = (t311+t312);
float t314 = (vf11/vf13);
float t315 = (t314*float(0.5));
float t316 = (t315+float(0.5));
float t317 = (1*t58);
float t318 = (t316+t317);
vec4 t319 = texture(s10,vec2(t313,t318));
bool t320 = (t308>t319.x);
float t321;
if(t320){
t321 = float(0.1);
} else {
t321 = float(1.0);
}
float t322 = (t291+t321);
float t323 = (t322/9);
float t324 = (t37*t323);
vec4 t325 = vec4(u8.u0,u8.u4,u8.u8,1);
vec4 t326 = vec4(t2,t4,0,0);
vec4 t327 = vec4(t6,t2,0,0);
vec4 t328 = vec4(0,0,1,0);
vec4 t329 = vec4(0,0,0,1);
mat4x4 t330 = mat4x4(t326,t327,t328,t329);
vec4 t331 = (t325*t330);
float t332 = (t331[0]-vf2);
float t333 = (t332*t332);
float t334 = (t331[1]-vf3);
float t335 = (t334*t334);
float t336 = (t333+t335);
float t337 = (t331[2]-vf4);
float t338 = (t337*t337);
float t339 = (t336+t338);
float t340 = (t331[3]-vf5);
float t341 = (t340*t340);
float t342 = (t339+t341);
float t343 = sqrt(t342);
float t344 = (t332/t343);
float t345 = (vf6*t344);
float t346 = (t334/t343);
float t347 = (vf7*t346);
float t348 = (t345+t347);
float t349 = (t337/t343);
float t350 = (vf8*t349);
float t351 = (t348+t350);
float t352 = (t340/t343);
float t353 = (vf9*t352);
float t354 = (t351+t353);
bool t355 = (t354>=0);
float t356;
if(t355){
t356 = t354;
} else {
t356 = 0;
}
float t357 = (u8.u12*t356);
float t358 = (vf16/vf17);
float t359 = (t358*float(0.5));
float t360 = (t359+float(0.5));
vec4 t361 = vec4(vf6,vf7,vf8,vf9);
float t362 = (t331[0]-vf2);
float t363 = (t331[1]-vf3);
float t364 = (t331[2]-vf4);
float t365 = (t331[3]-vf5);
vec4 t366 = vec4(t362,t363,t364,t365);
vec4 t367 = normalize(t366);
vec4 t368 = vec4(t367[0],t367[1],t367[2],t367[3]);
float t369 = dot(t361,t368);
float t370 = (float(1.0)-t369);
float t371 = (float(5.0e-4)*t370);
bool t372 = (t371>=float(5.0e-5));
float t373;
if(t372){
t373 = t371;
} else {
t373 = float(5.0e-5);
}
float t374 = (t360-t373);
float t375 = (vf14/vf17);
float t376 = (t375*float(0.5));
float t377 = (t376+float(0.5));
float t378 = (0*t58);
float t379 = (t377+t378);
float t380 = (vf15/vf17);
float t381 = (t380*float(0.5));
float t382 = (t381+float(0.5));
float t383 = (0*t58);
float t384 = (t382+t383);
vec4 t385 = texture(s11,vec2(t379,t384));
bool t386 = (t374>t385.x);
float t387;
if(t386){
t387 = float(0.1);
} else {
t387 = float(1.0);
}
float t388 = (0+t387);
float t389 = (vf16/vf17);
float t390 = (t389*float(0.5));
float t391 = (t390+float(0.5));
vec4 t392 = vec4(vf6,vf7,vf8,vf9);
float t393 = (t331[0]-vf2);
float t394 = (t331[1]-vf3);
float t395 = (t331[2]-vf4);
float t396 = (t331[3]-vf5);
vec4 t397 = vec4(t393,t394,t395,t396);
vec4 t398 = normalize(t397);
vec4 t399 = vec4(t398[0],t398[1],t398[2],t398[3]);
float t400 = dot(t392,t399);
float t401 = (float(1.0)-t400);
float t402 = (float(5.0e-4)*t401);
bool t403 = (t402>=float(5.0e-5));
float t404;
if(t403){
t404 = t402;
} else {
t404 = float(5.0e-5);
}
float t405 = (t391-t404);
float t406 = (vf14/vf17);
float t407 = (t406*float(0.5));
float t408 = (t407+float(0.5));
float t409 = (t90*t58);
float t410 = (t408+t409);
float t411 = (vf15/vf17);
float t412 = (t411*float(0.5));
float t413 = (t412+float(0.5));
float t414 = (t90*t58);
float t415 = (t413+t414);
vec4 t416 = texture(s11,vec2(t410,t415));
bool t417 = (t405>t416.x);
float t418;
if(t417){
t418 = float(0.1);
} else {
t418 = float(1.0);
}
float t419 = (t388+t418);
float t420 = (vf16/vf17);
float t421 = (t420*float(0.5));
float t422 = (t421+float(0.5));
vec4 t423 = vec4(vf6,vf7,vf8,vf9);
float t424 = (t331[0]-vf2);
float t425 = (t331[1]-vf3);
float t426 = (t331[2]-vf4);
float t427 = (t331[3]-vf5);
vec4 t428 = vec4(t424,t425,t426,t427);
vec4 t429 = normalize(t428);
vec4 t430 = vec4(t429[0],t429[1],t429[2],t429[3]);
float t431 = dot(t423,t430);
float t432 = (float(1.0)-t431);
float t433 = (float(5.0e-4)*t432);
bool t434 = (t433>=float(5.0e-5));
float t435;
if(t434){
t435 = t433;
} else {
t435 = float(5.0e-5);
}
float t436 = (t422-t435);
float t437 = (vf14/vf17);
float t438 = (t437*float(0.5));
float t439 = (t438+float(0.5));
float t440 = (t122*t58);
float t441 = (t439+t440);
float t442 = (vf15/vf17);
float t443 = (t442*float(0.5));
float t444 = (t443+float(0.5));
float t445 = (0*t58);
float t446 = (t444+t445);
vec4 t447 = texture(s11,vec2(t441,t446));
bool t448 = (t436>t447.x);
float t449;
if(t448){
t449 = float(0.1);
} else {
t449 = float(1.0);
}
float t450 = (t419+t449);
float t451 = (vf16/vf17);
float t452 = (t451*float(0.5));
float t453 = (t452+float(0.5));
vec4 t454 = vec4(vf6,vf7,vf8,vf9);
float t455 = (t331[0]-vf2);
float t456 = (t331[1]-vf3);
float t457 = (t331[2]-vf4);
float t458 = (t331[3]-vf5);
vec4 t459 = vec4(t455,t456,t457,t458);
vec4 t460 = normalize(t459);
vec4 t461 = vec4(t460[0],t460[1],t460[2],t460[3]);
float t462 = dot(t454,t461);
float t463 = (float(1.0)-t462);
float t464 = (float(5.0e-4)*t463);
bool t465 = (t464>=float(5.0e-5));
float t466;
if(t465){
t466 = t464;
} else {
t466 = float(5.0e-5);
}
float t467 = (t453-t466);
float t468 = (vf14/vf17);
float t469 = (t468*float(0.5));
float t470 = (t469+float(0.5));
float t471 = (t154*t58);
float t472 = (t470+t471);
float t473 = (vf15/vf17);
float t474 = (t473*float(0.5));
float t475 = (t474+float(0.5));
float t476 = (1*t58);
float t477 = (t475+t476);
vec4 t478 = texture(s11,vec2(t472,t477));
bool t479 = (t467>t478.x);
float t480;
if(t479){
t480 = float(0.1);
} else {
t480 = float(1.0);
}
float t481 = (t450+t480);
float t482 = (vf16/vf17);
float t483 = (t482*float(0.5));
float t484 = (t483+float(0.5));
vec4 t485 = vec4(vf6,vf7,vf8,vf9);
float t486 = (t331[0]-vf2);
float t487 = (t331[1]-vf3);
float t488 = (t331[2]-vf4);
float t489 = (t331[3]-vf5);
vec4 t490 = vec4(t486,t487,t488,t489);
vec4 t491 = normalize(t490);
vec4 t492 = vec4(t491[0],t491[1],t491[2],t491[3]);
float t493 = dot(t485,t492);
float t494 = (float(1.0)-t493);
float t495 = (float(5.0e-4)*t494);
bool t496 = (t495>=float(5.0e-5));
float t497;
if(t496){
t497 = t495;
} else {
t497 = float(5.0e-5);
}
float t498 = (t484-t497);
float t499 = (vf14/vf17);
float t500 = (t499*float(0.5));
float t501 = (t500+float(0.5));
float t502 = (0*t58);
float t503 = (t501+t502);
float t504 = (vf15/vf17);
float t505 = (t504*float(0.5));
float t506 = (t505+float(0.5));
float t507 = (t191*t58);
float t508 = (t506+t507);
vec4 t509 = texture(s11,vec2(t503,t508));
bool t510 = (t498>t509.x);
float t511;
if(t510){
t511 = float(0.1);
} else {
t511 = float(1.0);
}
float t512 = (t481+t511);
float t513 = (vf16/vf17);
float t514 = (t513*float(0.5));
float t515 = (t514+float(0.5));
vec4 t516 = vec4(vf6,vf7,vf8,vf9);
float t517 = (t331[0]-vf2);
float t518 = (t331[1]-vf3);
float t519 = (t331[2]-vf4);
float t520 = (t331[3]-vf5);
vec4 t521 = vec4(t517,t518,t519,t520);
vec4 t522 = normalize(t521);
vec4 t523 = vec4(t522[0],t522[1],t522[2],t522[3]);
float t524 = dot(t516,t523);
float t525 = (float(1.0)-t524);
float t526 = (float(5.0e-4)*t525);
bool t527 = (t526>=float(5.0e-5));
float t528;
if(t527){
t528 = t526;
} else {
t528 = float(5.0e-5);
}
float t529 = (t515-t528);
float t530 = (vf14/vf17);
float t531 = (t530*float(0.5));
float t532 = (t531+float(0.5));
float t533 = (0*t58);
float t534 = (t532+t533);
float t535 = (vf15/vf17);
float t536 = (t535*float(0.5));
float t537 = (t536+float(0.5));
float t538 = (1*t58);
float t539 = (t537+t538);
vec4 t540 = texture(s11,vec2(t534,t539));
bool t541 = (t529>t540.x);
float t542;
if(t541){
t542 = float(0.1);
} else {
t542 = float(1.0);
}
float t543 = (t512+t542);
float t544 = (vf16/vf17);
float t545 = (t544*float(0.5));
float t546 = (t545+float(0.5));
vec4 t547 = vec4(vf6,vf7,vf8,vf9);
float t548 = (t331[0]-vf2);
float t549 = (t331[1]-vf3);
float t550 = (t331[2]-vf4);
float t551 = (t331[3]-vf5);
vec4 t552 = vec4(t548,t549,t550,t551);
vec4 t553 = normalize(t552);
vec4 t554 = vec4(t553[0],t553[1],t553[2],t553[3]);
float t555 = dot(t547,t554);
float t556 = (float(1.0)-t555);
float t557 = (float(5.0e-4)*t556);
bool t558 = (t557>=float(5.0e-5));
float t559;
if(t558){
t559 = t557;
} else {
t559 = float(5.0e-5);
}
float t560 = (t546-t559);
float t561 = (vf14/vf17);
float t562 = (t561*float(0.5));
float t563 = (t562+float(0.5));
float t564 = (1*t58);
float t565 = (t563+t564);
float t566 = (vf15/vf17);
float t567 = (t566*float(0.5));
float t568 = (t567+float(0.5));
float t569 = (t254*t58);
float t570 = (t568+t569);
vec4 t571 = texture(s11,vec2(t565,t570));
bool t572 = (t560>t571.x);
float t573;
if(t572){
t573 = float(0.1);
} else {
t573 = float(1.0);
}
float t574 = (t543+t573);
float t575 = (vf16/vf17);
float t576 = (t575*float(0.5));
float t577 = (t576+float(0.5));
vec4 t578 = vec4(vf6,vf7,vf8,vf9);
float t579 = (t331[0]-vf2);
float t580 = (t331[1]-vf3);
float t581 = (t331[2]-vf4);
float t582 = (t331[3]-vf5);
vec4 t583 = vec4(t579,t580,t581,t582);
vec4 t584 = normalize(t583);
vec4 t585 = vec4(t584[0],t584[1],t584[2],t584[3]);
float t586 = dot(t578,t585);
float t587 = (float(1.0)-t586);
float t588 = (float(5.0e-4)*t587);
bool t589 = (t588>=float(5.0e-5));
float t590;
if(t589){
t590 = t588;
} else {
t590 = float(5.0e-5);
}
float t591 = (t577-t590);
float t592 = (vf14/vf17);
float t593 = (t592*float(0.5));
float t594 = (t593+float(0.5));
float t595 = (1*t58);
float t596 = (t594+t595);
float t597 = (vf15/vf17);
float t598 = (t597*float(0.5));
float t599 = (t598+float(0.5));
float t600 = (0*t58);
float t601 = (t599+t600);
vec4 t602 = texture(s11,vec2(t596,t601));
bool t603 = (t591>t602.x);
float t604;
if(t603){
t604 = float(0.1);
} else {
t604 = float(1.0);
}
float t605 = (t574+t604);
float t606 = (vf16/vf17);
float t607 = (t606*float(0.5));
float t608 = (t607+float(0.5));
vec4 t609 = vec4(vf6,vf7,vf8,vf9);
float t610 = (t331[0]-vf2);
float t611 = (t331[1]-vf3);
float t612 = (t331[2]-vf4);
float t613 = (t331[3]-vf5);
vec4 t614 = vec4(t610,t611,t612,t613);
vec4 t615 = normalize(t614);
vec4 t616 = vec4(t615[0],t615[1],t615[2],t615[3]);
float t617 = dot(t609,t616);
float t618 = (float(1.0)-t617);
float t619 = (float(5.0e-4)*t618);
bool t620 = (t619>=float(5.0e-5));
float t621;
if(t620){
t621 = t619;
} else {
t621 = float(5.0e-5);
}
float t622 = (t608-t621);
float t623 = (vf14/vf17);
float t624 = (t623*float(0.5));
float t625 = (t624+float(0.5));
float t626 = (1*t58);
float t627 = (t625+t626);
float t628 = (vf15/vf17);
float t629 = (t628*float(0.5));
float t630 = (t629+float(0.5));
float t631 = (1*t58);
float t632 = (t630+t631);
vec4 t633 = texture(s11,vec2(t627,t632));
bool t634 = (t622>t633.x);
float t635;
if(t634){
t635 = float(0.1);
} else {
t635 = float(1.0);
}
float t636 = (t605+t635);
float t637 = (t636/9);
float t638 = (t357*t637);
float t639 = (0+0);
float t640 = (0+t639);
float t641 = (0+t640);
float t642 = (0+t641);
float t643 = (0+t642);
float t644 = (0+t643);
float t645 = (0+t644);
float t646 = (t638+t645);
float t647 = (t324+t646);
float t648 = (u5.u24*t647);
float t649 = (vf0/100000);
float t650 = (vf1/100000);
vec4 t651 = texture(s12,vec2(t649,t650));
float t652 = (u6.u12*t651.x);
float t653 = (t648*t652);
float t654 = (u7.u16*t36);
float t655 = (t654*t323);
float t656 = (u8.u16*t356);
float t657 = (t656*t637);
float t658 = (0+0);
float t659 = (0+t658);
float t660 = (0+t659);
float t661 = (0+t660);
float t662 = (0+t661);
float t663 = (0+t662);
float t664 = (0+t663);
float t665 = (t657+t664);
float t666 = (t655+t665);
float t667 = (u5.u24*t666);
float t668 = (u6.u16*t651.x);
float t669 = (t667*t668);
float t670 = (u7.u20*t36);
float t671 = (t670*t323);
float t672 = (u8.u20*t356);
float t673 = (t672*t637);
float t674 = (0+0);
float t675 = (0+t674);
float t676 = (0+t675);
float t677 = (0+t676);
float t678 = (0+t677);
float t679 = (0+t678);
float t680 = (0+t679);
float t681 = (t673+t680);
float t682 = (t671+t681);
float t683 = (u5.u24*t682);
float t684 = (u6.u20*t651.x);
float t685 = (t683*t684);
out0 = vec4(t653,t669,t685,1);
vec3 t686 = vec3(t653,t669,t685);
vec3 t687 = vec3(float(0.2126),float(0.7152),float(7.22e-2));
float t688 = dot(t686,t687);
bool t689 = (t688>float(1.0));
float t690;
float t691;
float t692;
if(t689){
t690 = t653;
t691 = t669;
t692 = t685;
} else {
t690 = 0;
t691 = 0;
t692 = 0;
}
out1 = vec4(t690,t691,t692,1);
gl_FragDepth = gl_FragCoord[2];
}
