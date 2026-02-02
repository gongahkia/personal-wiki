import { useState, useCallback } from "react"

const characters = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!@#$%^&*()_+"

export function useSlotMachine(originalText: string) {
  const [text, setText] = useState(originalText)
  const [intervalId, setIntervalId] = useState<NodeJS.Timeout | null>(null)

  const startAnimation = useCallback(() => {
    if (intervalId) return

    const newIntervalId = setInterval(() => {
      setText((prev) =>
        prev
          .split("")
          .map((char, i) => (char === " " ? " " : characters[Math.floor(Math.random() * characters.length)]))
          .join(""),
      )
    }, 50)

    setIntervalId(newIntervalId)

    setTimeout(() => {
      if (newIntervalId) clearInterval(newIntervalId)
      setIntervalId(null)
      setText(originalText)
    }, 1000)
  }, [originalText, intervalId]) 

  const stopAnimation = useCallback(() => {
    if (intervalId) {
      clearInterval(intervalId)
      setIntervalId(null)
      setText(originalText)
    }
  }, [intervalId, originalText])

  return { text, startAnimation, stopAnimation }
}