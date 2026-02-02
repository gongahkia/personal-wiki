"use client"

import { motion } from "framer-motion"
import { CyclingPolygon } from "@/components/cycling-polygon"
import { AnimatedLink } from "@/components/animated-link"
import { useSlotMachine } from "@/hooks/useSlotMachine"

const links = [
  { href: "/about", label: "About" },
  { href: "https://github.com/gongahkia/personal-wiki", label: "Notes" },
  { href: "https://github.com/gongahkia", label: "Github" },
  { href: "https://gongzm.wordpress.com/", label: "Blog" },
  { href: "https://github.com/gongahkia/resume/blob/main/output/main.pdf", label: "Resume" },
  { href: "/secret", label: "Secret" },
  { href: "/contact", label: "Contact" },
]

export default function Home() {
  const { text, startAnimation, stopAnimation } = useSlotMachine("GABRIEL ONG")

  return (
    <main className="min-h-screen flex items-center justify-center bg-white px-2 py-4">
      <div className="flex items-center justify-between w-full max-w-sm">
        <CyclingPolygon className="w-12 h-12 text-gray-800" />

        <div className="flex flex-col items-center space-y-6 mx-2">
          <motion.h1
            className="text-3xl sm:text-4xl font-serif text-gray-800"
            initial={{ opacity: 0 }}
            animate={{ opacity: 1 }}
            transition={{ duration: 1 }}
            onHoverStart={startAnimation}
            onHoverEnd={stopAnimation}
          >
            {text}
          </motion.h1>

          <nav className="flex flex-col items-center space-y-1 w-full">
            {links.map((link) => (
              <AnimatedLink key={link.label} href={link.href} className="w-full">
                {link.label}
              </AnimatedLink>
            ))}
          </nav>
        </div>

        <CyclingPolygon className="w-12 h-12 text-gray-800" />
      </div>
    </main>
  )
}