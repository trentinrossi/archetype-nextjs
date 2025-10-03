'use client';

import { useState, useEffect, useCallback, useRef } from 'react';
import { Button } from '@/components/ui/Button';
import { Input } from '@/components/ui/Input';
import { Select } from '@/components/ui/Select';
import { userService } from '@/services/userService';
import { 
  UserFormData, 
  UserFormErrors, 
  CreateUserRequest,
  UserType,
  USER_TYPE_LABELS,
  USER_VALIDATION_RULES 
} from '@/types/user';

interface UserFormProps {
  onSave?: (userData: CreateUserRequest) => Promise<void>;
  onSaveAndExit?: (userData: CreateUserRequest) => Promise<void>;
  onCancel?: () => void;
  className?: string;
}

export default function UserForm({ 
  onSave, 
  onSaveAndExit, 
  onCancel, 
  className = '' 
}: UserFormProps) {
  const [formData, setFormData] = useState<UserFormData>({
    userId: '',
    firstName: '',
    lastName: '',
    password: '',
    confirmPassword: '',
    userType: UserType.USER,
  });

  const [errors, setErrors] = useState<UserFormErrors>({});
  const [loading, setLoading] = useState(false);
  const [message, setMessage] = useState<{ type: 'success' | 'error'; text: string } | null>(null);
  const [isDirty, setIsDirty] = useState(false);

  // Refs for keyboard navigation
  const userIdRef = useRef<HTMLInputElement>(null);
  const firstNameRef = useRef<HTMLInputElement>(null);
  const lastNameRef = useRef<HTMLInputElement>(null);
  const passwordRef = useRef<HTMLInputElement>(null);
  const confirmPasswordRef = useRef<HTMLInputElement>(null);
  const userTypeRef = useRef<HTMLSelectElement>(null);
  const saveButtonRef = useRef<HTMLButtonElement>(null);
  const clearButtonRef = useRef<HTMLButtonElement>(null);
  const saveExitButtonRef = useRef<HTMLButtonElement>(null);

  const fieldRefs = [
    userIdRef,
    firstNameRef,
    lastNameRef,
    passwordRef,
    confirmPasswordRef,
    userTypeRef,
    saveExitButtonRef,
    saveButtonRef,
    clearButtonRef,
  ];

  const validateField = useCallback((name: keyof UserFormData, value: string): string | undefined => {
    switch (name) {
      case 'userId':
        if (!value.trim()) {
          return 'User ID is required';
        }
        if (value.length > USER_VALIDATION_RULES.userId.maxLength) {
          return `User ID must be ${USER_VALIDATION_RULES.userId.maxLength} characters or less`;
        }
        if (!USER_VALIDATION_RULES.userId.pattern.test(value.toUpperCase())) {
          return 'User ID must contain only alphanumeric characters';
        }
        break;

      case 'firstName':
        if (!value.trim()) {
          return 'First Name is required';
        }
        if (value.length > USER_VALIDATION_RULES.firstName.maxLength) {
          return `First Name must be ${USER_VALIDATION_RULES.firstName.maxLength} characters or less`;
        }
        break;

      case 'lastName':
        if (!value.trim()) {
          return 'Last Name is required';
        }
        if (value.length > USER_VALIDATION_RULES.lastName.maxLength) {
          return `Last Name must be ${USER_VALIDATION_RULES.lastName.maxLength} characters or less`;
        }
        break;

      case 'password':
        if (!value) {
          return 'Password is required';
        }
        if (value.length !== USER_VALIDATION_RULES.password.minLength) {
          return `Password must be exactly ${USER_VALIDATION_RULES.password.minLength} characters`;
        }
        if (!USER_VALIDATION_RULES.password.pattern.test(value)) {
          return 'Password must contain uppercase, lowercase, number, and special character';
        }
        break;

      case 'confirmPassword':
        if (!value) {
          return 'Confirm Password is required';
        }
        if (value !== formData.password) {
          return 'Passwords do not match';
        }
        break;

      case 'userType':
        if (!value) {
          return 'User Type is required';
        }
        if (!USER_VALIDATION_RULES.userType.allowedValues.includes(value as UserType)) {
          return 'Invalid User Type';
        }
        break;

      default:
        break;
    }
    return undefined;
  }, [formData.password]);

  const validateForm = useCallback((): boolean => {
    const newErrors: UserFormErrors = {};
    let isValid = true;

    // Validate all fields
    Object.keys(formData).forEach((key) => {
      const fieldName = key as keyof UserFormData;
      const error = validateField(fieldName, formData[fieldName]);
      if (error) {
        newErrors[fieldName] = error;
        isValid = false;
      }
    });

    setErrors(newErrors);
    return isValid;
  }, [formData, validateField]);

  const handleInputChange = useCallback((name: keyof UserFormData, value: string) => {
    setFormData(prev => ({ ...prev, [name]: value }));
    setIsDirty(true);
    
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

  const handleClear = useCallback(() => {
    setFormData({
      userId: '',
      firstName: '',
      lastName: '',
      password: '',
      confirmPassword: '',
      userType: UserType.USER,
    });
    setErrors({});
    setMessage(null);
    setIsDirty(false);
    
    // Focus on first field
    setTimeout(() => {
      userIdRef.current?.focus();
    }, 0);
  }, []);

  const handleSave = useCallback(async (saveAndExit: boolean = false) => {
    try {
      setLoading(true);
      setMessage(null);
      setErrors({});

      if (!validateForm()) {
        setMessage({ type: 'error', text: 'Please correct the errors above' });
        return;
      }

      const userData: CreateUserRequest = {
        userId: formData.userId.toUpperCase().trim(),
        firstName: formData.firstName.trim(),
        lastName: formData.lastName.trim(),
        password: formData.password,
        userType: formData.userType,
      };

      if (saveAndExit && onSaveAndExit) {
        await onSaveAndExit(userData);
        setMessage({ type: 'success', text: 'User created successfully. Exiting...' });
      } else if (onSave) {
        await onSave(userData);
        setMessage({ type: 'success', text: 'User created successfully' });
        setIsDirty(false);
      } else {
        // Default behavior - call userService directly
        await userService.createUser(userData);
        setMessage({ type: 'success', text: 'User created successfully' });
        setIsDirty(false);
      }

    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : 'Failed to create user';
      setMessage({ type: 'error', text: errorMessage });
      setErrors({ general: errorMessage });
    } finally {
      setLoading(false);
    }
  }, [formData, validateForm, onSave, onSaveAndExit]);

  const handleKeyDown = useCallback((event: KeyboardEvent) => {
    switch (event.key) {
      case 'F3':
        event.preventDefault();
        handleSave(true); // Save and Exit
        break;
      case 'F4':
        event.preventDefault();
        handleClear();
        break;
      case 'F5':
        event.preventDefault();
        handleSave(false); // Save
        break;
      case 'Escape':
        if (onCancel) {
          event.preventDefault();
          onCancel();
        }
        break;
      case 'Tab':
        // Allow default tab behavior
        break;
      case 'Enter':
        event.preventDefault();
        const activeElement = document.activeElement;
        const currentIndex = fieldRefs.findIndex(ref => ref.current === activeElement);
        
        if (currentIndex >= 0 && currentIndex < fieldRefs.length - 1) {
          // Move to next field
          const nextRef = fieldRefs[currentIndex + 1];
          nextRef.current?.focus();
        } else if (currentIndex === fieldRefs.length - 1) {
          // Last field, trigger save
          handleSave(false);
        }
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
  }, [handleSave, handleClear, onCancel, fieldRefs]);

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
    // Focus on first field when component mounts
    setTimeout(() => {
      userIdRef.current?.focus();
    }, 100);
  }, []);

  const userTypeOptions = Object.values(UserType).map(type => ({
    value: type,
    label: USER_TYPE_LABELS[type],
  }));

  return (
    <div className={`max-w-2xl mx-auto p-6 bg-white border rounded-lg shadow-sm ${className}`}>
      {/* Header */}
      <div className="mb-6">
        <h1 className="text-2xl font-bold text-gray-900">Add New User (COUSR01C)</h1>
        <p className="text-sm text-gray-600 mt-1">
          Create a new user account with required information
        </p>
      </div>

      {/* Success/Error Messages */}
      {message && (
        <div className={`mb-6 p-4 rounded-md ${
          message.type === 'success' 
            ? 'bg-green-50 border border-green-200 text-green-800' 
            : 'bg-red-50 border border-red-200 text-red-800'
        }`}>
          <p className="font-medium">{message.text}</p>
        </div>
      )}

      {/* General Error */}
      {errors.general && (
        <div className="mb-6 p-4 bg-red-50 border border-red-200 rounded-md">
          <p className="text-red-800 font-medium">Error: {errors.general}</p>
        </div>
      )}

      {/* Form */}
      <form onSubmit={(e) => e.preventDefault()} className="space-y-6">
        {/* User ID */}
        <div>
          <Input
            ref={userIdRef}
            label="User ID *"
            value={formData.userId}
            onChange={(e) => handleInputChange('userId', e.target.value.toUpperCase())}
            error={errors.userId}
            placeholder="Enter User ID (max 8 chars)"
            maxLength={8}
            disabled={loading}
            className="font-mono"
          />
          <p className="text-xs text-gray-500 mt-1">
            Maximum 8 alphanumeric characters
          </p>
        </div>

        {/* First Name */}
        <div>
          <Input
            ref={firstNameRef}
            label="First Name *"
            value={formData.firstName}
            onChange={(e) => handleInputChange('firstName', e.target.value)}
            error={errors.firstName}
            placeholder="Enter First Name"
            maxLength={20}
            disabled={loading}
          />
          <p className="text-xs text-gray-500 mt-1">
            Maximum 20 characters
          </p>
        </div>

        {/* Last Name */}
        <div>
          <Input
            ref={lastNameRef}
            label="Last Name *"
            value={formData.lastName}
            onChange={(e) => handleInputChange('lastName', e.target.value)}
            error={errors.lastName}
            placeholder="Enter Last Name"
            maxLength={20}
            disabled={loading}
          />
          <p className="text-xs text-gray-500 mt-1">
            Maximum 20 characters
          </p>
        </div>

        {/* Password */}
        <div>
          <Input
            ref={passwordRef}
            label="Password *"
            type="password"
            value={formData.password}
            onChange={(e) => handleInputChange('password', e.target.value)}
            error={errors.password}
            placeholder="Enter Password"
            maxLength={8}
            disabled={loading}
            className="font-mono"
          />
          <p className="text-xs text-gray-500 mt-1">
            Exactly 8 characters with uppercase, lowercase, number, and special character
          </p>
        </div>

        {/* Confirm Password */}
        <div>
          <Input
            ref={confirmPasswordRef}
            label="Confirm Password *"
            type="password"
            value={formData.confirmPassword}
            onChange={(e) => handleInputChange('confirmPassword', e.target.value)}
            error={errors.confirmPassword}
            placeholder="Confirm Password"
            maxLength={8}
            disabled={loading}
            className="font-mono"
          />
        </div>

        {/* User Type */}
        <div>
          <Select
            ref={userTypeRef}
            label="User Type *"
            value={formData.userType}
            onChange={(value) => handleInputChange('userType', value)}
            options={userTypeOptions}
            error={errors.userType}
            disabled={loading}
          />
          <p className="text-xs text-gray-500 mt-1">
            Select the appropriate user type
          </p>
        </div>

        {/* Action Buttons */}
        <div className="flex flex-wrap gap-3 pt-6 border-t">
          <Button
            ref={saveExitButtonRef}
            onClick={() => handleSave(true)}
            disabled={loading || !isDirty}
            className="min-w-[140px]"
          >
            {loading ? 'Saving...' : 'Save & Exit (F3)'}
          </Button>

          <Button
            ref={saveButtonRef}
            onClick={() => handleSave(false)}
            disabled={loading || !isDirty}
            variant="secondary"
            className="min-w-[120px]"
          >
            {loading ? 'Saving...' : 'Save (F5)'}
          </Button>

          <Button
            ref={clearButtonRef}
            onClick={handleClear}
            disabled={loading}
            variant="outline"
            className="min-w-[120px]"
          >
            Clear (F4)
          </Button>

          {onCancel && (
            <Button
              onClick={onCancel}
              disabled={loading}
              variant="outline"
            >
              Cancel (Esc)
            </Button>
          )}
        </div>
      </form>

      {/* Navigation Instructions */}
      <div className="mt-8 p-4 bg-gray-50 rounded-md">
        <h3 className="text-sm font-medium text-gray-900 mb-2">Keyboard Navigation:</h3>
        <div className="grid grid-cols-1 md:grid-cols-2 gap-2 text-xs text-gray-600">
          <div>F3: Save and Exit</div>
          <div>F4: Clear Form</div>
          <div>F5: Save</div>
          <div>Esc: Cancel</div>
          <div>Tab/Enter: Next Field</div>
          <div>↑/↓: Navigate Fields</div>
        </div>
      </div>

      {/* Form Status */}
      <div className="mt-4 text-xs text-gray-500 text-center">
        {isDirty && <span className="text-orange-600">● Form has unsaved changes</span>}
        {!isDirty && formData.userId && <span className="text-green-600">● Form is saved</span>}
      </div>
    </div>
  );
}