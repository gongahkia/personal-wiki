"use client"

import { useEffect, useState, useRef } from "react"
import { animate, useMotionValue } from "framer-motion"

export function CyclingPolygon({ className = "" }: { className?: string }) {
  const [sides, setSides] = useState(6)
  const progress = useMotionValue(6)
  const polygonRefs = useRef<SVGPolygonElement[]>([])

  useEffect(() => {
    const interval = setInterval(() => {
      setSides((prev) => (prev === 12 ? 3 : prev + 1))
    }, 1200) // Slower rotation
    return () => clearInterval(interval)
  }, [])

  const getPolygonPoints = (sides: number) => {
    const points = []
    const radius = 45
    const centerX = 50
    const centerY = 50

    for (let i = 0; i < Math.max(3, sides); i++) {
      const angle = (i * 2 * Math.PI) / sides - Math.PI / 2
      const x = centerX + radius * Math.cos(angle)
      const y = centerY + radius * Math.sin(angle)
      points.push(`${x},${y}`)
    }

    return points.join(" ")
  }

  useEffect(() => {
    animate(progress, sides, {
      duration: 1.2,
      ease: "easeInOut",
      onUpdate: (latest) => {
        polygonRefs.current.forEach((ref) => {
          if (ref) {
            ref.setAttribute("points", getPolygonPoints(latest))
          }
        })
      },
    })
  }, [sides, progress])

  return (
    <svg viewBox="0 0 100 100" className={className} fill="none" stroke="currentColor" strokeWidth="2">
      <g>
        {[...Array(8)].map((_, i) => (
          <polygon
            key={i}
            ref={(el) => (polygonRefs.current[i] = el as SVGPolygonElement)}
            points={getPolygonPoints(sides)}
            strokeWidth={(i + 1) * 0.5}
            opacity={1 - i * 0.1}
            style={{ transform: `scale(${1 - i * 0.1})` }}
          />
        ))}
      </g>
    </svg>
  )
}