"use client"

import { motion } from "framer-motion"
import { CyclingPolygon } from "@/components/cycling-polygon"
import Link from "next/link"
import { useState, useEffect } from "react"

export default function Secret() {
  const [secret, setSecret] = useState("Hash.")

  useEffect(() => {
    const generateSecret = async () => {
      const message = "If you decrypt this, send me the salt on LinkedIn."
      const salt = "gongahkia"
      const combined = salt + message
      const encoder = new TextEncoder()
      const data = encoder.encode(combined)
      const hash = await crypto.subtle.digest("SHA-256", data)
      const hashArray = Array.from(new Uint8Array(hash))
      const hashHex = hashArray.map((b) => b.toString(16).padStart(2, "0")).join("")
      setSecret(hashHex)
    }
    generateSecret()
  }, [])

  return (
    <main className="min-h-screen flex items-center justify-center bg-white px-2 py-4">
      <div className="flex items-center justify-between w-full max-w-sm">
        <Link href="/">
          <CyclingPolygon className="w-12 h-12 text-gray-800" />
        </Link>

        <motion.div
          className="mx-2"
          initial={{ opacity: 0, y: 20 }}
          animate={{ opacity: 1, y: 0 }}
          transition={{ duration: 0.8 }}
        >
          <p className="text-center font-mono text-xs sm:text-sm break-all">{secret}</p>

          <footer className="text-center text-gray-600 font-serif text-xs mt-4">
            <p>© {new Date().getFullYear()} Gabriel Ong / All rights reserved.</p>
          </footer>
        </motion.div>

        <div>
          <CyclingPolygon className="w-12 h-12 text-gray-800" />
        </div>
      </div>
    </main>
  )
}