"use client"

import { motion } from "framer-motion"
import { useSlotMachine } from "@/hooks/useSlotMachine"
import type React from "react" 

interface ContentCardProps {
  title: string
  content: React.ReactNode
  className?: string
}

export function ContentCard({ title, content, className = "" }: ContentCardProps) {
  const { text, startAnimation, stopAnimation } = useSlotMachine(title)

  return (
    <motion.div
      className={`bg-white/50 backdrop-blur-sm p-6 rounded-lg shadow-lg ${className}`}
      whileHover={{ scale: 1.02 }}
      onHoverStart={startAnimation}
      onHoverEnd={stopAnimation}
    >
      <h2 className="text-2xl mb-4 font-serif text-gray-800">{text}</h2>
      <div className="prose prose-gray max-w-none">{content}</div>
    </motion.div>
  )
}