'use client';

import { useState, useEffect, useCallback, useRef } from 'react';
import { Button } from '@/components/ui/Button';
import { Input } from '@/components/ui/Input';
import { userService } from '@/services/userService';
import { 
  LoginFormData, 
  LoginFormErrors, 
  UserLoginRequest,
  UserLoginResponse,
  USER_VALIDATION_RULES 
} from '@/types/user';

interface LoginFormProps {
  onLogin?: (response: UserLoginResponse) => Promise<void>;
  onExit?: () => void;
  className?: string;
}

export default function LoginForm({ 
  onLogin, 
  onExit, 
  className = '' 
}: LoginFormProps) {
  const [formData, setFormData] = useState<LoginFormData>({
    userId: '',
    password: '',
  });

  const [errors, setErrors] = useState<LoginFormErrors>({});
  const [loading, setLoading] = useState(false);
  const [message, setMessage] = useState<{ type: 'success' | 'error' | 'info'; text: string } | null>(null);

  // Refs for keyboard navigation
  const userIdRef = useRef<HTMLInputElement>(null);
  const passwordRef = useRef<HTMLInputElement>(null);
  const loginButtonRef = useRef<HTMLButtonElement>(null);
  const exitButtonRef = useRef<HTMLButtonElement>(null);

  const fieldRefs = [
    userIdRef,
    passwordRef,
    loginButtonRef,
    exitButtonRef,
  ];

  const validateField = useCallback((name: keyof LoginFormData, value: string): string | undefined => {
    switch (name) {
      case 'userId':
        if (!value.trim()) {
          return 'Please enter User ID';
        }
        if (value.length > USER_VALIDATION_RULES.userId.maxLength) {
          return `User ID must be ${USER_VALIDATION_RULES.userId.maxLength} characters or less`;
        }
        break;

      case 'password':
        if (!value) {
          return 'Please enter Password';
        }
        if (value.length > USER_VALIDATION_RULES.password.maxLength) {
          return `Password must be ${USER_VALIDATION_RULES.password.maxLength} characters or less`;
        }
        break;

      default:
        break;
    }
    return undefined;
  }, []);

  const validateForm = useCallback((): boolean => {
    const newErrors: LoginFormErrors = {};
    let isValid = true;

    // Validate all fields
    Object.keys(formData).forEach((key) => {
      const fieldName = key as keyof LoginFormData;
      const error = validateField(fieldName, formData[fieldName]);
      if (error) {
        newErrors[fieldName] = error;
        isValid = false;
      }
    });

    setErrors(newErrors);
    return isValid;
  }, [formData, validateField]);

  const handleInputChange = useCallback((name: keyof LoginFormData, value: string) => {
    setFormData(prev => ({ ...prev, [name]: value }));
    
    // Clear field error when user starts typing
    if (errors[name]) {
      setErrors(prev => ({ ...prev, [name]: undefined }));
    }
    
    // Clear general error
    if (errors.general) {
      setErrors(prev => ({ ...prev, general: undefined }));
    }
    
    // Clear message
    if (message) {
      setMessage(null);
    }
  }, [errors, message]);

  const handleLogin = useCallback(async () => {
    try {
      setLoading(true);
      setMessage(null);
      setErrors({});

      if (!validateForm()) {
        setMessage({ type: 'error', text: 'Please correct the errors above' });
        return;
      }

      const credentials: UserLoginRequest = {
        userId: formData.userId.toUpperCase().trim(),
        password: formData.password,
      };

      const response = await userService.authenticateUser(credentials);
      
      setMessage({ type: 'success', text: 'Login successful!' });
      
      if (onLogin) {
        await onLogin(response);
      }

    } catch (error) {
      let errorMessage = 'Authentication failed';
      
      if (error instanceof Error) {
        const msg = error.message.toLowerCase();
        if (msg.includes('password')) {
          errorMessage = 'Wrong Password';
          setErrors({ password: errorMessage });
        } else if (msg.includes('not found') || msg.includes('user')) {
          errorMessage = 'User not found';
          setErrors({ userId: errorMessage });
        } else if (msg.includes('inactive')) {
          errorMessage = 'User account is inactive';
          setErrors({ general: errorMessage });
        } else {
          errorMessage = error.message;
        }
      }
      
      setMessage({ type: 'error', text: errorMessage });
      setErrors(prev => ({ ...prev, general: errorMessage }));
    } finally {
      setLoading(false);
    }
  }, [formData, validateForm, onLogin]);

  const handleExit = useCallback(() => {
    setMessage({ type: 'info', text: 'Thank you for using CardDemo Application' });
    
    if (onExit) {
      setTimeout(() => {
        onExit();
      }, 1500);
    }
  }, [onExit]);

  const handleKeyDown = useCallback((event: KeyboardEvent) => {
    switch (event.key) {
      case 'Enter':
        event.preventDefault();
        const activeElement = document.activeElement;
        const currentIndex = fieldRefs.findIndex(ref => ref.current === activeElement);
        
        if (currentIndex >= 0 && currentIndex < fieldRefs.length - 2) {
          // Move to next field
          const nextRef = fieldRefs[currentIndex + 1];
          nextRef.current?.focus();
        } else {
          // Last field or no specific field, trigger login
          handleLogin();
        }
        break;
      case 'F3':
        event.preventDefault();
        handleExit();
        break;
      case 'Escape':
        event.preventDefault();
        handleExit();
        break;
      case 'Tab':
        // Allow default tab behavior
        break;
      case 'ArrowDown':
        event.preventDefault();
        const activeDownElement = document.activeElement;
        const currentDownIndex = fieldRefs.findIndex(ref => ref.current === activeDownElement);
        
        if (currentDownIndex >= 0 && currentDownIndex < fieldRefs.length - 1) {
          const nextRef = fieldRefs[currentDownIndex + 1];
          nextRef.current?.focus();
        }
        break;
      case 'ArrowUp':
        event.preventDefault();
        const activeUpElement = document.activeElement;
        const currentUpIndex = fieldRefs.findIndex(ref => ref.current === activeUpElement);
        
        if (currentUpIndex > 0) {
          const prevRef = fieldRefs[currentUpIndex - 1];
          prevRef.current?.focus();
        }
        break;
    }
  }, [handleLogin, handleExit, fieldRefs]);

  useEffect(() => {
    const handleGlobalKeyDown = (event: KeyboardEvent) => {
      handleKeyDown(event);
    };

    window.addEventListener('keydown', handleGlobalKeyDown);
    return () => {
      window.removeEventListener('keydown', handleGlobalKeyDown);
    };
  }, [handleKeyDown]);

  useEffect(() => {
    // Focus on User ID field when component mounts
    setTimeout(() => {
      userIdRef.current?.focus();
    }, 100);
  }, []);

  return (
    <div className={`max-w-md mx-auto p-8 bg-white border rounded-lg shadow-lg ${className}`}>
      {/* Header */}
      <div className="text-center mb-8">
        <h1 className="text-3xl font-bold text-gray-900 mb-2">CardDemo</h1>
        <h2 className="text-xl font-semibold text-gray-700 mb-1">User Sign-on (COSGN00C)</h2>
        <p className="text-sm text-gray-600">
          Please enter your credentials to access the system
        </p>
      </div>

      {/* Success/Error Messages */}
      {message && (
        <div className={`mb-6 p-4 rounded-md ${
          message.type === 'success' 
            ? 'bg-green-50 border border-green-200 text-green-800' 
            : message.type === 'info'
            ? 'bg-blue-50 border border-blue-200 text-blue-800'
            : 'bg-red-50 border border-red-200 text-red-800'
        }`}>
          <p className="font-medium text-center">{message.text}</p>
        </div>
      )}

      {/* General Error */}
      {errors.general && (
        <div className="mb-6 p-4 bg-red-50 border border-red-200 rounded-md">
          <p className="text-red-800 font-medium text-center">{errors.general}</p>
        </div>
      )}

      {/* Login Form */}
      <form onSubmit={(e) => e.preventDefault()} className="space-y-6">
        {/* User ID */}
        <div>
          <Input
            ref={userIdRef}
            label="User ID"
            value={formData.userId}
            onChange={(e) => handleInputChange('userId', e.target.value.toUpperCase())}
            error={errors.userId}
            placeholder="Enter your User ID"
            maxLength={8}
            disabled={loading}
            className="font-mono text-center"
            autoComplete="username"
          />
        </div>

        {/* Password */}
        <div>
          <Input
            ref={passwordRef}
            label="Password"
            type="password"
            value={formData.password}
            onChange={(e) => handleInputChange('password', e.target.value)}
            error={errors.password}
            placeholder="Enter your Password"
            maxLength={8}
            disabled={loading}
            className="font-mono text-center"
            autoComplete="current-password"
          />
        </div>

        {/* Action Buttons */}
        <div className="space-y-3 pt-4">
          <Button
            ref={loginButtonRef}
            onClick={handleLogin}
            disabled={loading || !formData.userId.trim() || !formData.password}
            className="w-full"
            size="lg"
          >
            {loading ? 'Signing in...' : 'Sign In (Enter)'}
          </Button>

          <Button
            ref={exitButtonRef}
            onClick={handleExit}
            disabled={loading}
            variant="outline"
            className="w-full"
            size="lg"
          >
            Exit (F3)
          </Button>
        </div>
      </form>

      {/* Navigation Instructions */}
      <div className="mt-8 p-4 bg-gray-50 rounded-md">
        <h3 className="text-sm font-medium text-gray-900 mb-2 text-center">Instructions:</h3>
        <div className="grid grid-cols-1 gap-1 text-xs text-gray-600 text-center">
          <div>Enter: Sign In / Next Field</div>
          <div>F3: Exit Application</div>
          <div>↑/↓: Navigate Fields</div>
        </div>
      </div>

      {/* System Information */}
      <div className="mt-6 text-xs text-gray-400 text-center">
        <div>Program: COSGN00C</div>
        <div>Transaction: CC00</div>
        <div>Date: {new Date().toLocaleDateString()}</div>
        <div>Time: {new Date().toLocaleTimeString()}</div>
      </div>
    </div>
  );
}