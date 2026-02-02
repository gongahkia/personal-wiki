"use client"

import { motion } from "framer-motion"
import Link from "next/link"
import { useSlotMachine } from "@/hooks/useSlotMachine"

interface AnimatedLinkProps {
  href: string
  children: string
  className?: string
}

export function AnimatedLink({ href, children, className = "" }: AnimatedLinkProps) {
  const { text, startAnimation, stopAnimation } = useSlotMachine(children)

  return (
    <motion.div
      className={`w-full ${className}`}
      whileHover={{ scale: 1.02 }}
      onHoverStart={startAnimation}
      onHoverEnd={stopAnimation}
    >
      <Link
        href={href}
        className="block py-4 text-center text-gray-600 hover:text-black transition-colors duration-300 font-serif text-xl"
      >
        {text}
      </Link>
    </motion.div>
  )
}