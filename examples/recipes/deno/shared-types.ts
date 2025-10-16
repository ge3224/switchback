/**
 * Shared TypeScript Types for Recipe Manager
 *
 * These types are imported by BOTH server.ts and app.ts, ensuring
 * complete type safety from server to client with zero build step.
 *
 * This is Deno's killer feature: TypeScript everywhere, no compilation needed!
 */

export interface Recipe {
  id: string;
  title: string;
  description: string;
  author: string;
  prepTime: number; // minutes
  cookTime: number; // minutes
  servings: number;
  difficulty: 'easy' | 'medium' | 'hard';
  ingredients: Ingredient[];
  steps: string[];
  tags: string[];
  nutrition: NutritionInfo;
  imageUrl?: string;
  createdAt: Date;
}

export interface Ingredient {
  name: string;
  amount: number;
  unit: MeasurementUnit;
}

export type MeasurementUnit =
  | 'cup'
  | 'tbsp'
  | 'tsp'
  | 'oz'
  | 'lb'
  | 'g'
  | 'kg'
  | 'ml'
  | 'l'
  | 'whole'
  | 'pinch';

export interface NutritionInfo {
  calories: number;
  protein: number; // grams
  carbs: number;   // grams
  fat: number;     // grams
}

/**
 * Page Props - These are what the server returns to Switchback
 */

export interface RecipeBookPageProps {
  recipes: Recipe[];
  featuredRecipe: Recipe;
  popularTags: string[];
}

export interface RecipeDetailPageProps {
  recipe: Recipe;
  relatedRecipes: Recipe[];
}

export interface RecipeSearchPageProps {
  recipes: Recipe[];
  query: string;
  selectedTags: string[];
  selectedDifficulty?: 'easy' | 'medium' | 'hard';
}

export interface AddRecipePageProps {
  tags: string[];
}

/**
 * Utility type guards for runtime validation
 */

export function isValidDifficulty(value: unknown): value is Recipe['difficulty'] {
  return value === 'easy' || value === 'medium' || value === 'hard';
}

export function isValidMeasurementUnit(value: unknown): value is MeasurementUnit {
  const validUnits: MeasurementUnit[] = [
    'cup', 'tbsp', 'tsp', 'oz', 'lb', 'g', 'kg', 'ml', 'l', 'whole', 'pinch'
  ];
  return typeof value === 'string' && validUnits.includes(value as MeasurementUnit);
}

/**
 * Helper functions that work on both client and server
 */

export function getTotalTime(recipe: Recipe): number {
  return recipe.prepTime + recipe.cookTime;
}

export function formatTime(minutes: number): string {
  if (minutes < 60) {
    return `${minutes} min`;
  }
  const hours = Math.floor(minutes / 60);
  const mins = minutes % 60;
  return mins > 0 ? `${hours}h ${mins}m` : `${hours}h`;
}

export function getDifficultyColor(difficulty: Recipe['difficulty']): string {
  switch (difficulty) {
    case 'easy': return '#10b981'; // green
    case 'medium': return '#f59e0b'; // amber
    case 'hard': return '#ef4444'; // red
  }
}
