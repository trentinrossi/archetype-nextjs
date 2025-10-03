'use client';

import { useState, useEffect, useCallback, useRef } from 'react';
import { Button } from '@/components/ui/Button';
import { Input } from '@/components/ui/Input';
import { Select } from '@/components/ui/Select';
import { userService } from '@/services/userService';
import { 
  UserResponse,
  UpdateUserRequest,
  UserFormErrors, 
  UserType,
  USER_TYPE_LABELS,
  USER_VALIDATION_RULES 
} from '@/types/user';

interface UserUpdateProps {
  initialUserId?: string;
  onUpdate?: (userId: string, userData: UpdateUserRequest) => Promise<void>;
  onUpdateAndExit?: (userId: string, userData: UpdateUserRequest) => Promise<void>;
  onCancel?: () => void;
  className?: string;
}

interface UpdateFormData {
  userId: string;
  firstName: string;
  lastName: string;
  password: string;
  userType: UserType;
}

export default function UserUpdate({ 
  initialUserId = '',
  onUpdate, 
  onUpdateAndExit, 
  onCancel, 
  className = '' 
}: UserUpdateProps) {
  const [formData, setFormData] = useState<UpdateFormData>({
    userId: initialUserId,
    firstName: '',
    lastName: '',
    password: '',
    userType: UserType.USER,
  });

  const [originalData, setOriginalData] = useState<UpdateFormData | null>(null);
  const [errors, setErrors] = useState<UserFormErrors>({});
  const [loading, setLoading] = useState(false);
  const [lookupLoading, setLookupLoading] = useState(false);
  const [message, setMessage] = useState<{ type: 'success' | 'error' | 'info'; text: string } | null>(null);
  const [userFound, setUserFound] = useState(false);
  const [hasChanges, setHasChanges] = useState(false);

  // Refs for keyboard navigation
  const userIdRef = useRef<HTMLInputElement>(null);
  const firstNameRef = useRef<HTMLInputElement>(null);
  const lastNameRef = useRef<HTMLInputElement>(null);
  const passwordRef = useRef<HTMLInputElement>(null);
  const userTypeRef = useRef<HTMLSelectElement>(null);
  const updateButtonRef = useRef<HTMLButtonElement>(null);
  const clearButtonRef = useRef<HTMLButtonElement>(null);
  const updateExitButtonRef = useRef<HTMLButtonElement>(null);
  const cancelButtonRef = useRef<HTMLButtonElement>(null);

  const fieldRefs = [
    userIdRef,
    firstNameRef,
    lastNameRef,
    passwordRef,
    userTypeRef,
    updateExitButtonRef,
    updateButtonRef,
    clearButtonRef,
    cancelButtonRef,
  ];

  const validateField = useCallback((name: keyof UpdateFormData, value: string): string | undefined => {
    switch (name) {
      case 'userId':
        if (!value.trim()) {
          return 'User ID is required';
        }
        if (value.length > USER_VALIDATION_RULES.userId.maxLength) {
          return `User ID must be ${USER_VALIDATION_RULES.userId.maxLength} characters or less`;
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
  }, []);

  const validateForm = useCallback((): boolean => {
    const newErrors: UserFormErrors = {};
    let isValid = true;

    // Only validate editable fields if user is found
    if (userFound) {
      ['firstName', 'lastName', 'password', 'userType'].forEach((key) => {
        const fieldName = key as keyof UpdateFormData;
        const error = validateField(fieldName, formData[fieldName]);
        if (error) {
          newErrors[fieldName] = error;
          isValid = false;
        }
      });
    } else {
      // Validate userId for lookup
      const userIdError = validateField('userId', formData.userId);
      if (userIdError) {
        newErrors.userId = userIdError;
        isValid = false;
      }
    }

    setErrors(newErrors);
    return isValid;
  }, [formData, validateField, userFound]);

  const checkForChanges = useCallback(() => {
    if (!originalData) {
      setHasChanges(false);
      return;
    }

    const changed = 
      formData.firstName !== originalData.firstName ||
      formData.lastName !== originalData.lastName ||
      formData.password !== originalData.password ||
      formData.userType !== originalData.userType;

    setHasChanges(changed);
  }, [formData, originalData]);

  const handleInputChange = useCallback((name: keyof UpdateFormData, value: string) => {
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

  const handleUserLookup = useCallback(async () => {
    if (!formData.userId.trim()) {
      setErrors({ userId: 'User ID is required' });
      setMessage({ type: 'error', text: 'User ID can NOT be empty...' });
      return;
    }

    try {
      setLookupLoading(true);
      setMessage(null);
      setErrors({});

      const user: UserResponse = await userService.getUserById(formData.userId);
      
      const userData: UpdateFormData = {
        userId: user.userId,
        firstName: user.firstName,
        lastName: user.lastName,
        password: '', // Don't show existing password
        userType: user.userType,
      };

      setFormData(userData);
      setOriginalData(userData);
      setUserFound(true);
      setMessage({ type: 'info', text: 'Press PF5 key to save your updates...' });
      
      // Focus on first editable field
      setTimeout(() => {
        firstNameRef.current?.focus();
      }, 0);

    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : 'User lookup failed';
      setMessage({ type: 'error', text: errorMessage.includes('not found') ? 'User ID NOT found...' : 'Unable to lookup User...' });
      setErrors({ userId: errorMessage });
      setUserFound(false);
      setOriginalData(null);
    } finally {
      setLookupLoading(false);
    }
  }, [formData.userId]);

  const handleClear = useCallback(() => {
    setFormData({
      userId: '',
      firstName: '',
      lastName: '',
      password: '',
      userType: UserType.USER,
    });
    setOriginalData(null);
    setErrors({});
    setMessage(null);
    setUserFound(false);
    setHasChanges(false);
    
    // Focus on User ID field
    setTimeout(() => {
      userIdRef.current?.focus();
    }, 0);
  }, []);

  const handleUpdate = useCallback(async (updateAndExit: boolean = false) => {
    try {
      setLoading(true);
      setMessage(null);
      setErrors({});

      if (!userFound) {
        setMessage({ type: 'error', text: 'Please lookup a user first' });
        return;
      }

      if (!validateForm()) {
        setMessage({ type: 'error', text: 'Please correct the errors above' });
        return;
      }

      if (!hasChanges) {
        setMessage({ type: 'error', text: 'Please modify to update...' });
        return;
      }

      const updateData: UpdateUserRequest = {
        firstName: formData.firstName.trim(),
        lastName: formData.lastName.trim(),
        password: formData.password,
        userType: formData.userType,
      };

      if (updateAndExit && onUpdateAndExit) {
        await onUpdateAndExit(formData.userId, updateData);
        setMessage({ type: 'success', text: `User ${formData.userId} has been updated. Exiting...` });
      } else if (onUpdate) {
        await onUpdate(formData.userId, updateData);
        setMessage({ type: 'success', text: `User ${formData.userId} has been updated...` });
      } else {
        // Default behavior - call userService directly
        await userService.updateUser(formData.userId, updateData);
        setMessage({ type: 'success', text: `User ${formData.userId} has been updated...` });
      }

      // Update original data to reflect changes
      setOriginalData({ ...formData });
      setHasChanges(false);

    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : 'Failed to update user';
      setMessage({ type: 'error', text: errorMessage.includes('not found') ? 'User ID NOT found...' : 'Unable to Update User...' });
      setErrors({ general: errorMessage });
    } finally {
      setLoading(false);
    }
  }, [formData, validateForm, userFound, hasChanges, onUpdate, onUpdateAndExit]);

  const handleKeyDown = useCallback((event: KeyboardEvent) => {
    switch (event.key) {
      case 'Enter':
        if (!userFound && formData.userId.trim()) {
          event.preventDefault();
          handleUserLookup();
        }
        break;
      case 'F3':
        event.preventDefault();
        if (userFound) {
          handleUpdate(true); // Update and Exit
        }
        break;
      case 'F4':
        event.preventDefault();
        handleClear();
        break;
      case 'F5':
        event.preventDefault();
        if (userFound) {
          handleUpdate(false); // Update
        }
        break;
      case 'F12':
        if (onCancel) {
          event.preventDefault();
          onCancel();
        }
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
  }, [formData.userId, userFound, handleUserLookup, handleUpdate, handleClear, onCancel, fieldRefs]);

  useEffect(() => {
    checkForChanges();
  }, [checkForChanges]);

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

  useEffect(() => {
    // Auto-lookup if initialUserId is provided
    if (initialUserId && !userFound) {
      handleUserLookup();
    }
  }, [initialUserId, userFound, handleUserLookup]);

  const userTypeOptions = Object.values(UserType).map(type => ({
    value: type,
    label: USER_TYPE_LABELS[type],
  }));

  return (
    <div className={`max-w-2xl mx-auto p-6 bg-white border rounded-lg shadow-sm ${className}`}>
      {/* Header */}
      <div className="mb-6">
        <h1 className="text-2xl font-bold text-gray-900">Update User (COUSR02C)</h1>
        <p className="text-sm text-gray-600 mt-1">
          Lookup and update user information
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
        {/* User ID Lookup */}
        <div>
          <div className="flex gap-2">
            <Input
              ref={userIdRef}
              label="User ID *"
              value={formData.userId}
              onChange={(e) => handleInputChange('userId', e.target.value.toUpperCase())}
              error={errors.userId}
              placeholder="Enter User ID to lookup"
              maxLength={8}
              disabled={loading || lookupLoading || userFound}
              className="font-mono flex-1"
            />
            {!userFound && (
              <Button
                onClick={handleUserLookup}
                disabled={loading || lookupLoading || !formData.userId.trim()}
                className="mt-6"
              >
                {lookupLoading ? 'Looking up...' : 'Lookup (Enter)'}
              </Button>
            )}
          </div>
          <p className="text-xs text-gray-500 mt-1">
            Enter User ID and press Enter or click Lookup
          </p>
        </div>

        {/* User Details (only shown after successful lookup) */}
        {userFound && (
          <>
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
                placeholder="Enter new password"
                maxLength={8}
                disabled={loading}
                className="font-mono"
              />
              <p className="text-xs text-gray-500 mt-1">
                Exactly 8 characters
              </p>
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
                ref={updateExitButtonRef}
                onClick={() => handleUpdate(true)}
                disabled={loading || !hasChanges}
                className="min-w-[140px]"
              >
                {loading ? 'Updating...' : 'Update & Exit (F3)'}
              </Button>

              <Button
                ref={updateButtonRef}
                onClick={() => handleUpdate(false)}
                disabled={loading || !hasChanges}
                variant="secondary"
                className="min-w-[120px]"
              >
                {loading ? 'Updating...' : 'Update (F5)'}
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
                  ref={cancelButtonRef}
                  onClick={onCancel}
                  disabled={loading}
                  variant="outline"
                >
                  Cancel (F12)
                </Button>
              )}
            </div>
          </>
        )}
      </form>

      {/* Navigation Instructions */}
      <div className="mt-8 p-4 bg-gray-50 rounded-md">
        <h3 className="text-sm font-medium text-gray-900 mb-2">Keyboard Navigation:</h3>
        <div className="grid grid-cols-1 md:grid-cols-2 gap-2 text-xs text-gray-600">
          <div>Enter: Lookup User</div>
          <div>F3: Update and Exit</div>
          <div>F4: Clear Form</div>
          <div>F5: Update</div>
          <div>F12: Cancel</div>
          <div>↑/↓: Navigate Fields</div>
        </div>
      </div>

      {/* Form Status */}
      <div className="mt-4 text-xs text-gray-500 text-center">
        {userFound && hasChanges && <span className="text-orange-600">● Form has unsaved changes</span>}
        {userFound && !hasChanges && <span className="text-green-600">● No changes detected</span>}
        {!userFound && <span className="text-gray-400">● Enter User ID to begin</span>}
      </div>
    </div>
  );
}