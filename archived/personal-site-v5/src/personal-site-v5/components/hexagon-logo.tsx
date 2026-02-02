export function HexagonLogo({ className = "" }: { className?: string }) {
  return (
    <svg viewBox="0 0 100 100" className={className} fill="none" stroke="currentColor" strokeWidth="2">
      <g>
        {[...Array(8)].map((_, i) => (
          <polygon
            key={i}
            points="50,10 90,30 90,70 50,90 10,70 10,30"
            strokeWidth={(i + 1) * 0.5}
            opacity={1 - i * 0.1}
            transform={`scale(${1 - i * 0.1})`}
          />
        ))}
      </g>
    </svg>
  )
}