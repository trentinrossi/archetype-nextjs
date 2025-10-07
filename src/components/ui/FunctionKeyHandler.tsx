'use client';

import React, { useEffect } from 'react';

interface FunctionKeyMapping {
  [key: string]: () => void;
}

interface FunctionKeyHandlerProps {
  keyMappings: FunctionKeyMapping;
  children: React.ReactNode;
  disabled?: boolean;
}

/**
 * Component to handle COBOL-style function key mappings
 * Simulates the behavior of COBOL programs that respond to function keys
 */
export function FunctionKeyHandler({ keyMappings, children, disabled = false }: FunctionKeyHandlerProps) {
  useEffect(() => {
    if (disabled) return;

    const handleKeyDown = (event: KeyboardEvent) => {
      // Handle F-keys (F1-F12)
      if (event.key.startsWith('F') && event.key.length <= 3) {
        const fKey = event.key;
        if (keyMappings[fKey]) {
          event.preventDefault();
          keyMappings[fKey]();
          return;
        }
      }

      // Handle Alt+Number combinations (simulating PF keys)
      if (event.altKey && event.key >= '1' && event.key <= '9') {
        const pfKey = `PF${event.key}`;
        if (keyMappings[pfKey]) {
          event.preventDefault();
          keyMappings[pfKey]();
          return;
        }
      }

      // Handle Alt+F combinations for F10-F12
      if (event.altKey && event.key.startsWith('F')) {
        const pfKey = `PF${event.key.substring(1)}`;
        if (keyMappings[pfKey]) {
          event.preventDefault();
          keyMappings[pfKey]();
          return;
        }
      }

      // Handle Enter key
      if (event.key === 'Enter' && keyMappings['ENTER']) {
        // Only handle if not in an input field or if explicitly mapped
        const target = event.target as HTMLElement;
        if (target.tagName !== 'INPUT' && target.tagName !== 'TEXTAREA') {
          event.preventDefault();
          keyMappings['ENTER']();
          return;
        }
      }

      // Handle Escape key (often mapped to PF3 or exit)
      if (event.key === 'Escape' && keyMappings['ESCAPE']) {
        event.preventDefault();
        keyMappings['ESCAPE']();
        return;
      }

      // Handle specific key combinations
      if (event.ctrlKey && event.key === 'c' && keyMappings['CTRL+C']) {
        event.preventDefault();
        keyMappings['CTRL+C']();
        return;
      }
    };

    // Add event listener to document
    document.addEventListener('keydown', handleKeyDown);

    // Cleanup
    return () => {
      document.removeEventListener('keydown', handleKeyDown);
    };
  }, [keyMappings, disabled]);

  return <>{children}</>;
}

/**
 * Hook to create standard COBOL function key mappings
 */
export function useCobolFunctionKeys() {
  const createKeyMappings = (handlers: {
    onEnter?: () => void;
    onPF3?: () => void;
    onPF4?: () => void;
    onPF5?: () => void;
    onPF7?: () => void;
    onPF8?: () => void;
    onPF12?: () => void;
    onEscape?: () => void;
    onInvalidKey?: () => void;
  }): FunctionKeyMapping => {
    const mappings: FunctionKeyMapping = {};

    if (handlers.onEnter) mappings['ENTER'] = handlers.onEnter;
    if (handlers.onPF3) {
      mappings['F3'] = handlers.onPF3;
      mappings['PF3'] = handlers.onPF3;
    }
    if (handlers.onPF4) {
      mappings['F4'] = handlers.onPF4;
      mappings['PF4'] = handlers.onPF4;
    }
    if (handlers.onPF5) {
      mappings['F5'] = handlers.onPF5;
      mappings['PF5'] = handlers.onPF5;
    }
    if (handlers.onPF7) {
      mappings['F7'] = handlers.onPF7;
      mappings['PF7'] = handlers.onPF7;
    }
    if (handlers.onPF8) {
      mappings['F8'] = handlers.onPF8;
      mappings['PF8'] = handlers.onPF8;
    }
    if (handlers.onPF12) {
      mappings['F12'] = handlers.onPF12;
      mappings['PF12'] = handlers.onPF12;
    }
    if (handlers.onEscape) {
      mappings['ESCAPE'] = handlers.onEscape;
    }

    // Handle invalid keys (F1, F2, F6, F9, F10, F11 if not mapped)
    const invalidKeyHandler = handlers.onInvalidKey || (() => {
      console.log('Invalid key pressed');
    });

    ['F1', 'F2', 'F6', 'F9', 'F10', 'F11'].forEach(key => {
      if (!mappings[key]) {
        mappings[key] = invalidKeyHandler;
      }
    });

    return mappings;
  };

  return { createKeyMappings };
}

/**
 * Component to display function key help
 */
interface FunctionKeyHelpProps {
  keys: Array<{
    key: string;
    description: string;
    enabled?: boolean;
  }>;
  className?: string;
}

export function FunctionKeyHelp({ keys, className = '' }: FunctionKeyHelpProps) {
  return (
    <div className={`text-gray-500 text-sm ${className}`}>
      <p className="font-semibold mb-2">Function Keys:</p>
      <div className="grid grid-cols-1 md:grid-cols-2 gap-1">
        {keys.map(({ key, description, enabled = true }) => (
          <div key={key} className={`flex justify-between ${enabled ? '' : 'opacity-50'}`}>
            <span className="font-mono">{key}:</span>
            <span className="ml-2">{description}</span>
          </div>
        ))}
      </div>
    </div>
  );
}